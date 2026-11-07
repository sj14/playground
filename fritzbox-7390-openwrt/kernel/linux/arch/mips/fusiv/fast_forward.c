// SPDX-License-Identifier: GPL-2.0
/*
 * Sends packets between the LAN port and the radios without the bridge.
 *
 * The CPU is too slow to reach the speed of the AVM firmware on the full path
 * through the network stack. So the receive paths of the LAN port and of mac80211
 * call this code first. It keeps its own short table of which address was last
 * seen on which interface, and sends a packet on directly if it knows the
 * destination.
 *
 * Everything else takes the normal path through the bridge: packets other than
 * IP, such as ARP and EAPOL, multicast, and addresses that were not seen lately.
 * So an address that moves to another interface or goes away only costs the
 * normal path. Filters on the bridge don't see the packets sent from here.
 *
 * The table is only used from softirq context on this single CPU, so it needs no
 * lock.
 */

#include <linux/etherdevice.h>
#include <linux/fusiv_fast_forward.h>
#include <linux/if_ether.h>
#include <linux/rcupdate.h>

#define KNOWN_ADDRESS_SLOTS	64
#define KNOWN_ADDRESS_LIFETIME	(5 * HZ)

struct known_address {
	u8 address[ETH_ALEN];
	int ifindex;
	unsigned long last_seen;
};

static struct known_address known_addresses[KNOWN_ADDRESS_SLOTS];

/* Two addresses in the same slot only send the older one down the normal path. */
static struct known_address *known_address_slot(const u8 *address)
{
	return &known_addresses[(address[4] ^ address[5]) % KNOWN_ADDRESS_SLOTS];
}

static void learn_address(const u8 *address, const struct net_device *dev)
{
	struct known_address *known = known_address_slot(address);

	if (is_multicast_ether_addr(address))
		return;
	ether_addr_copy(known->address, address);
	known->ifindex = dev->ifindex;
	known->last_seen = jiffies;
}

static struct net_device *find_destination(const u8 *address, const struct net_device *from)
{
	const struct known_address *known = known_address_slot(address);
	struct net_device *to;

	if (!ether_addr_equal(known->address, address) || known->ifindex == from->ifindex ||
	    time_after(jiffies, known->last_seen + KNOWN_ADDRESS_LIFETIME))
		return NULL;

	to = dev_get_by_index_rcu(dev_net(from), known->ifindex);
	if (!to || !netif_running(to) || !(to->priv_flags & IFF_BRIDGE_PORT))
		return NULL;
	return to;
}

bool fusiv_fast_forward(struct sk_buff *skb, struct net_device *from)
{
	const struct ethhdr *header = (const struct ethhdr *)skb->data;
	struct net_device *to;

	if (skb_headlen(skb) < ETH_HLEN || !(from->priv_flags & IFF_BRIDGE_PORT))
		return false;
	if (header->h_proto != htons(ETH_P_IP) && header->h_proto != htons(ETH_P_IPV6))
		return false;

	learn_address(header->h_source, from);

	rcu_read_lock();
	to = find_destination(header->h_dest, from);
	if (to) {
		skb->dev = to;
		skb->protocol = header->h_proto;
		skb_reset_mac_header(skb);
		skb_set_network_header(skb, ETH_HLEN);
		/*
		 * Neither driver has a queue in front of it or locks that the network
		 * stack must hold, and mac80211 picks the Wi-Fi queue by itself.
		 */
		netdev_start_xmit(skb, to, netdev_get_tx_queue(to, 0), false);
	}
	rcu_read_unlock();
	return to != NULL;
}
