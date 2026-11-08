/* SPDX-License-Identifier: GPL-2.0 */
#ifndef _LINUX_FUSIV_FAST_FORWARD_H
#define _LINUX_FUSIV_FAST_FORWARD_H

#include <linux/netdevice.h>
#include <linux/skbuff.h>

/*
 * Takes a received frame whose data starts at the Ethernet header. Returns true
 * if the frame was sent on, and false if it should take the normal path.
 */
#if IS_BUILTIN(CONFIG_BRIDGE)
bool fusiv_fast_forward(struct sk_buff *skb, struct net_device *from);
#else
static inline bool fusiv_fast_forward(struct sk_buff *skb, struct net_device *from)
{
	return false;
}
#endif

#endif
