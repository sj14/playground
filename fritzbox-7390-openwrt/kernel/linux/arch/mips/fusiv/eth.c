// SPDX-License-Identifier: GPL-2.0
/*
 * Ethernet through the Ikanos packet processors (APs) of the Fusiv Vx180.
 *
 * The Ethernet block is not reachable directly. Two small processors move the
 * packets: the buffer manager (BMU) holds a pool of free buffers, and the MAC
 * processor passes packets between the wire, buffers and queues in RAM.
 *
 * The register layout and configuration blocks were worked out from AVM's
 * firmware, see the project document. The firmware itself comes from AVM's
 * modules, see tools/extract-ap-firmware.py.
 */

#include <linux/delay.h>
#include <linux/dma-mapping.h>
#include <linux/firmware.h>
#include <linux/fusiv_fast_forward.h>
#include <linux/hash.h>
#include <linux/init.h>
#include <linux/io.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/interrupt.h>
#include <linux/of.h>
#include <linux/ip.h>
#include <linux/platform_device.h>
#include <linux/slab.h>
#include <linux/timer.h>
#include <net/ip.h>

#define BMU_ID			5
#define MAC1_ID			1
#define BMU_BASE		0x19210000
#define MAC1_BASE		0x19260000
#define SHARED_MEMORY_BASE	0x191f0000
#define MAC_QUEUE_INDEXES	0x40

#define AP_CONTROL		0x300
#define AP_CONTROL_RESET	0x8000
#define AP_CONTROL_RUN		0x4000
#define AP_INTERRUPT_STATUS	0x308
#define AP_INTERRUPT_MASK	0x30c
#define AP_CONFIG_HIGH		0x310
#define AP_CONFIG_LOW		0x314
#define AP_CONFIG_EVENT		0x318
#define AP_PROGRAM_ADDRESS	0x330
#define AP_PROGRAM_DATA		0x340
#define AP_ENABLE_0		0x500
#define AP_QUEUE_STATUS		0x504
#define AP_INPUT_FIFO		0x510
#define AP_ENABLE_1		0x600
#define AP_ENABLE_2		0x700
#define AP_ENABLE_3		0x800
#define AP_ENABLE_4		0x900
#define AP_DATA_MEMORY		0x8000
#define BMU_FREE_BUFFER_FIFO	0x510
#define BMU_TAKE_BUFFER		0x810

#define CONFIG_EVENT_NEW	1
#define EGRESS_HOST		0xffffffff
#define MAC_HOST_QUEUE		1
#define MAC_INTERRUPT_MASK	0xb000
#define CONFIG_WAIT_LIMIT_MS	15000

#define CONFIG_BYTES		352
#define QUEUE_COUNT		4
#define QUEUE_ENTRIES		504
#define FLOW_TABLE_ENTRIES	256
#define FLOW_TABLE_ENTRY_BYTES	192
#define BMU_POOL_MINIMUM	4096
#define BMU_POOL_MAXIMUM	4352
#define BMU_RAM_QUEUE_ENTRIES	(BMU_POOL_MAXIMUM - 128)
#define BUFFER_BYTES		2304
#define INPUT_QUEUE_COUNT	8
#define INPUT_QUEUE_ENTRIES	100
#define SHORT_BUFFER_BYTES	256

#define MAC_REGISTERS		0x19110000
#define MAC_REGISTERS_BYTES	0x1000
#define MAC_RX_FRAMES		0x940
#define MAC_RX_BYTES		0x948
#define MAC_TX_FRAMES		0x994
#define MAC_TX_BYTES		0x99c

#define STATUS_BUFFER_REQUEST	0x1000
#define BUFFER_SERVICE_INTERVAL	1
/* The fields of the header at the start of each buffer that this driver uses. */
#define BUFFER_START_OFFSET	10
#define BUFFER_END_OFFSET	12

#define MONITOR_INTERVAL	(10 * HZ)
#define MAC_ALL_INTERRUPTS_MASKED 0xffff
#define NAPI_BUDGET		64
#define TX_QUEUE_LEVEL_MASK	0xf0
#define TX_QUEUE_LEVEL_LIMIT	96
#define TX_DATA_START		192
#define HEADER_BYTES		64
#define BUFFER_FLAGS2_OFFSET	6
#define BUFFER_LENGTH_OFFSET	16
#define BUFFER_EGRESS_OFFSET	24
#define BUFFER_FLAG_TX		0x8000
#define STATUS_QUOTA_REQUEST	0x0010
#define AP_QUOTA_ANSWER		0x31c
#define AP_QUOTA_ANSWER_DONE	0xffff
#define CONFIG_EVENT_QUOTA	0x10
#define QUOTA_OFFSET		244
#define QUOTA_COUNT		8
#define QUOTA_PACKETS		64
#define PHYSICAL_ADDRESS_MASK	0x1fffffff

/* The layout of a firmware file from tools/extract-ap-firmware.py. */
struct ap_firmware_header {
	__be32 code_bytes;
	__be32 data_bytes;
	__be32 small_data_bytes;
	__be32 start_data;
	__be32 end_data;
	__be32 start_small_data;
	__be32 end_small_data;
	__be32 start_bss;
	__be32 end_bss;
	__be32 start_small_bss;
	__be32 end_small_bss;
};

struct ap_memory {
	void *cpu;
	dma_addr_t bus;
};

struct fusiv_eth {
	struct device *dev;
	struct net_device *netdev;
	struct timer_list monitor;
	void __iomem *bmu;
	void __iomem *mac;
	void __iomem *shared;
	void __iomem *mac_registers;
	struct timer_list buffer_service;
	u32 *mac_queues[QUEUE_COUNT];
	unsigned int buffers_given;
	struct napi_struct napi;
	int irq;
	bool receiving;
	struct ap_memory mac_config;
	unsigned int quota_requests;
	bool started;
};

static int ap_alloc(struct fusiv_eth *eth, struct ap_memory *memory, size_t bytes)
{
	memory->cpu = dma_alloc_coherent(eth->dev, bytes, &memory->bus, GFP_KERNEL);
	return memory->cpu ? 0 : -ENOMEM;
}

static void config_write32(struct ap_memory *config, unsigned int offset, u32 value)
{
	*(__be32 *)(config->cpu + offset) = cpu_to_be32(value);
}

static void config_write16(struct ap_memory *config, unsigned int offset, u16 value)
{
	*(__be16 *)(config->cpu + offset) = cpu_to_be16(value);
}

static void log_ap_state(struct fusiv_eth *eth, const char *name, void __iomem *ap)
{
	netdev_info(eth->netdev, "%s: control 0x%04x status 0x%04x mask 0x%04x event 0x%04x queue 0x%04x\n",
		    name, __raw_readw(ap + AP_CONTROL), __raw_readw(ap + AP_INTERRUPT_STATUS),
		    __raw_readw(ap + AP_INTERRUPT_MASK), __raw_readw(ap + AP_CONFIG_EVENT),
		    __raw_readw(ap + AP_QUEUE_STATUS));
}

static void ap_load_program(void __iomem *ap, const u8 *code, u32 bytes)
{
	u32 offset;

	__raw_writew(0, ap + AP_PROGRAM_ADDRESS);
	for (offset = 0; offset < bytes; offset += 2)
		__raw_writew((code[offset] << 8) | code[offset + 1], ap + AP_PROGRAM_DATA);
}

static void ap_load_data(void __iomem *ap, u32 address, const u8 *data, u32 bytes)
{
	u32 offset;

	for (offset = 0; offset < bytes; offset++)
		__raw_writeb(data[offset], ap + AP_DATA_MEMORY + address + offset);
}

static void ap_clear_data(void __iomem *ap, u32 start, u32 end)
{
	u32 address;

	for (address = start; address < end; address++)
		__raw_writeb(0, ap + AP_DATA_MEMORY + address);
}

/* These buffers can become socket buffers without a copy, see build_skb(). */
static void *buffer_alloc(void)
{
	return netdev_alloc_frag_align(BUFFER_BYTES, SMP_CACHE_BYTES);
}

static void bmu_give_buffer(struct fusiv_eth *eth, void *buffer)
{
	dma_cache_wback_inv((unsigned long)buffer, BUFFER_BYTES);
	__raw_writel(virt_to_phys(buffer) & PHYSICAL_ADDRESS_MASK, eth->bmu + BMU_FREE_BUFFER_FIFO);
}

/* The buffer manager asks for each free buffer it needs. */
static void bmu_serve_requests(struct fusiv_eth *eth)
{
	while (__raw_readw(eth->bmu + AP_INTERRUPT_STATUS) & STATUS_BUFFER_REQUEST) {
		void *buffer;

		if (eth->buffers_given >= BMU_POOL_MAXIMUM)
			return;
		buffer = buffer_alloc();
		if (!buffer)
			return;
		bmu_give_buffer(eth, buffer);
		eth->buffers_given++;
		__raw_writew(STATUS_BUFFER_REQUEST, eth->bmu + AP_INTERRUPT_STATUS);
	}
}

static int ap_send_config(struct fusiv_eth *eth, const char *name, void __iomem *ap,
			  struct ap_memory *config)
{
	unsigned long deadline = jiffies + msecs_to_jiffies(CONFIG_WAIT_LIMIT_MS);

	__raw_writew(config->bus >> 16, ap + AP_CONFIG_HIGH);
	__raw_writew(config->bus & 0xffff, ap + AP_CONFIG_LOW);
	__raw_writew(CONFIG_EVENT_NEW, ap + AP_CONFIG_EVENT);

	while (time_before(jiffies, deadline)) {
		if (!(__raw_readw(ap + AP_CONFIG_EVENT) & CONFIG_EVENT_NEW))
			return 0;
		bmu_serve_requests(eth);
		usleep_range(50, 100);
	}
	netdev_err(eth->netdev, "%s: no answer to its configuration, %u buffers given\n", name,
		   eth->buffers_given);
	return -ETIMEDOUT;
}

/* Loads a processor's firmware and starts it with a configuration block. */
static int ap_start(struct fusiv_eth *eth, const char *name, const char *firmware_name,
		    void __iomem *ap, bool is_bmu, struct ap_memory *config)
{
	const struct ap_firmware_header *header;
	const struct firmware *firmware;
	const u8 *section;
	int ret;

	ret = request_firmware(&firmware, firmware_name, eth->dev);
	if (ret) {
		netdev_err(eth->netdev, "%s: no firmware %s: %d\n", name, firmware_name, ret);
		return ret;
	}
	header = (const struct ap_firmware_header *)firmware->data;
	section = firmware->data + sizeof(*header);

	__raw_writew(AP_CONTROL_RESET, ap + AP_CONTROL);
	__raw_writew(0, ap + AP_CONTROL);

	ap_load_program(ap, section, be32_to_cpu(header->code_bytes));
	section += be32_to_cpu(header->code_bytes);
	ap_load_data(ap, be32_to_cpu(header->start_data), section, be32_to_cpu(header->data_bytes));
	section += be32_to_cpu(header->data_bytes);
	ap_load_data(ap, be32_to_cpu(header->start_small_data), section,
		     be32_to_cpu(header->small_data_bytes));
	ap_clear_data(ap, be32_to_cpu(header->start_bss), be32_to_cpu(header->end_bss));
	ap_clear_data(ap, be32_to_cpu(header->start_small_bss), be32_to_cpu(header->end_small_bss));
	release_firmware(firmware);

	__raw_writel(1, ap + AP_ENABLE_0);
	if (is_bmu) {
		__raw_writel(1, ap + AP_ENABLE_1);
		__raw_writel(1, ap + AP_ENABLE_2);
		__raw_writel(0, ap + AP_ENABLE_3);
		__raw_writel(0, ap + AP_ENABLE_4);
	}

	/* The MAC processor's interrupts stay masked until the interface is open. */
	__raw_writew(is_bmu ? (u16)~STATUS_BUFFER_REQUEST : MAC_ALL_INTERRUPTS_MASKED,
		     ap + AP_INTERRUPT_MASK);
	__raw_writew(AP_CONTROL_RUN, ap + AP_CONTROL);
	netdev_info(eth->netdev, "%s: firmware loaded and started\n", name);
	log_ap_state(eth, name, ap);

	ret = ap_send_config(eth, name, ap, config);
	log_ap_state(eth, name, ap);
	return ret;
}

static int ap_alloc_queues(struct fusiv_eth *eth, struct ap_memory *config, unsigned int offset,
			   u32 **queues)
{
	struct ap_memory queue;
	unsigned int i;
	int ret;

	for (i = 0; i < QUEUE_COUNT; i++) {
		ret = ap_alloc(eth, &queue, QUEUE_ENTRIES * sizeof(u32));
		if (ret)
			return ret;
		config_write32(config, offset + i * 8, queue.bus);
		config_write16(config, offset + i * 8 + 4, QUEUE_ENTRIES);
		if (queues)
			queues[i] = queue.cpu;
	}
	return 0;
}

static int ap_alloc_common(struct fusiv_eth *eth, struct ap_memory *config)
{
	struct ap_memory table;
	int ret;

	ret = ap_alloc(eth, config, CONFIG_BYTES);
	if (ret)
		return ret;

	ret = ap_alloc(eth, &table, FLOW_TABLE_ENTRIES * FLOW_TABLE_ENTRY_BYTES);
	if (ret)
		return ret;
	config_write32(config, 8, table.bus);
	config_write16(config, 12, FLOW_TABLE_ENTRIES - 1);

	ret = ap_alloc(eth, &table, FLOW_TABLE_ENTRIES * FLOW_TABLE_ENTRY_BYTES);
	if (ret)
		return ret;
	config_write32(config, 24, table.bus);
	config_write16(config, 28, FLOW_TABLE_ENTRIES - 1);

	/*
	 * AVM sends packets on to a third processor for filtering (0x19100510).
	 * It doesn't run here, so all packets go to the host instead.
	 */
	config_write32(config, 36, EGRESS_HOST);
	config_write32(config, 44, EGRESS_HOST);
	config_write16(config, 76, 1);
	return 0;
}

static int bmu_start(struct fusiv_eth *eth)
{
	struct ap_memory config, ram_queue, short_buffer;
	int ret;

	ret = ap_alloc_common(eth, &config);
	if (ret)
		return ret;

	ret = ap_alloc(eth, &ram_queue, BMU_RAM_QUEUE_ENTRIES * sizeof(u32));
	if (ret)
		return ret;
	ret = ap_alloc(eth, &short_buffer, SHORT_BUFFER_BYTES);
	if (ret)
		return ret;

	config_write32(&config, 164, ram_queue.bus);
	config_write16(&config, 168, BMU_RAM_QUEUE_ENTRIES);
	config_write16(&config, 170, 32);
	config_write16(&config, 172, BMU_POOL_MINIMUM - 113);
	config_write32(&config, 176, BMU_BASE + BMU_TAKE_BUFFER);
	config_write32(&config, 180, BMU_BASE + BMU_FREE_BUFFER_FIFO);
	config_write32(&config, 184, short_buffer.bus);
	config_write16(&config, 188, 0x8060);
	config_write32(&config, 192, 0x190a0003);
	config_write32(&config, 196, 0x190a0017);
	config_write32(&config, 336, SHARED_MEMORY_BASE);

	ret = ap_alloc_queues(eth, &config, 304, NULL);
	if (ret)
		return ret;

	return ap_start(eth, "bmu", "fusiv-bmu.bin", eth->bmu, true, &config);
}

static int mac_processor_start(struct fusiv_eth *eth)
{
	struct ap_memory config, short_buffer, input_queue;
	unsigned int i;
	int ret;

	ret = ap_alloc_common(eth, &config);
	if (ret)
		return ret;
	eth->mac_config = config;
	for (i = 0; i < QUOTA_COUNT; i++)
		config_write16(&config, QUOTA_OFFSET + i * 2, QUOTA_PACKETS);
	ret = ap_alloc(eth, &short_buffer, SHORT_BUFFER_BYTES);
	if (ret)
		return ret;

	config_write32(&config, 40, MAC_HOST_QUEUE);
	config_write32(&config, 48, MAC_HOST_QUEUE);
	config_write32(&config, 160, BMU_BASE + BMU_TAKE_BUFFER);
	config_write32(&config, 164, BMU_BASE + BMU_FREE_BUFFER_FIFO);
	config_write32(&config, 168, short_buffer.bus);
	config_write16(&config, 172, 0x8060);
	config_write32(&config, 176, MAC1_BASE + AP_INPUT_FIFO);
	config_write32(&config, 324, SHARED_MEMORY_BASE + MAC_QUEUE_INDEXES);
	*(u8 *)(config.cpu + 288) = 1;

	for (i = 0; i < INPUT_QUEUE_COUNT; i++) {
		ret = ap_alloc(eth, &input_queue, INPUT_QUEUE_ENTRIES * 32);
		if (ret)
			return ret;
		config_write32(&config, 180 + i * 8, input_queue.bus);
		config_write16(&config, 184 + i * 8, INPUT_QUEUE_ENTRIES);
	}

	ret = ap_alloc_queues(eth, &config, 292, eth->mac_queues);
	if (ret)
		return ret;

	return ap_start(eth, "mac", "fusiv-eth.bin", eth->mac, false, &config);
}

struct mac_setting {
	u16 offset;
	u32 value;
};

/*
 * The values of the running AVM firmware, read with tools/fusiv-mdio.c. Their
 * meaning is mostly unknown. The MAC counters in the same range are left out.
 */
static const struct mac_setting MAC_SETTINGS[] = {
	{0x880, 0x0000fc0c}, {0x884, 0x00000001}, {0x88c, 0x0000000a}, {0x89c, 0x00000003},
	{0x8a4, 0x00000001}, {0x8a8, 0x00002009}, {0x8c8, 0x00000040}, {0x8cc, 0x000006c0},
	{0x8d0, 0x0000019a}, {0x8e8, 0x00000fff}, {0x8ec, 0x00000804}, {0x8f0, 0x00005555},
	{0x908, 0x55545657}, {0x90c, 0xaaabacad}, {0x930, 0x00000210}, {0xb00, 0x02000400},
	{0xb04, 0x00800100}, {0xb1c, 0x026c026c}, {0xb20, 0x03600360},
};

static void mac_setup(struct fusiv_eth *eth)
{
	unsigned int i;

	for (i = 0; i < ARRAY_SIZE(MAC_SETTINGS); i++)
		__raw_writel(MAC_SETTINGS[i].value, eth->mac_registers + MAC_SETTINGS[i].offset);
}

static void buffer_service_tick(struct timer_list *timer)
{
	struct fusiv_eth *eth = timer_container_of(eth, timer, buffer_service);

	bmu_serve_requests(eth);
	/* AVM's interrupt counter stays near zero, so don't depend on the interrupt. */
	if (eth->receiving)
		napi_schedule(&eth->napi);
	mod_timer(&eth->buffer_service, jiffies + BUFFER_SERVICE_INTERVAL);
}

static void monitor_tick(struct timer_list *timer)
{
	struct fusiv_eth *eth = timer_container_of(eth, timer, monitor);
	struct net_device_stats *stats = &eth->netdev->stats;

	netdev_info(eth->netdev, "rx %lu drop %lu tx %lu drop %lu, %u buffers given, %u quota requests, control 0x%04x status 0x%04x\n",
		    stats->rx_packets, stats->rx_dropped, stats->tx_packets, stats->tx_dropped,
		    eth->buffers_given, eth->quota_requests, __raw_readw(eth->mac + AP_CONTROL),
		    __raw_readw(eth->mac + AP_INTERRUPT_STATUS));
	netdev_info(eth->netdev, "mac counters: rx 0x%x frames 0x%x bytes, tx 0x%x frames 0x%x bytes\n",
		    __raw_readl(eth->mac_registers + MAC_RX_FRAMES),
		    __raw_readl(eth->mac_registers + MAC_RX_BYTES),
		    __raw_readl(eth->mac_registers + MAC_TX_FRAMES),
		    __raw_readl(eth->mac_registers + MAC_TX_BYTES));
	mod_timer(&eth->monitor, jiffies + MONITOR_INTERVAL);
}

/*
 * mac80211 sorts packets into flows by this hash and otherwise works it out with
 * the flow dissector and SipHash, which costs 5% of the CPU during a download.
 */
static void set_flow_hash(struct sk_buff *skb)
{
	const struct ethhdr *ethernet = (const struct ethhdr *)skb->data;
	const struct iphdr *ip = (const struct iphdr *)(skb->data + ETH_HLEN);
	const __be32 *ports = (const __be32 *)(ip + 1);

	if (skb->len < ETH_HLEN + sizeof(*ip) + sizeof(*ports))
		return;
	if (ethernet->h_proto != htons(ETH_P_IP) || ip->ihl != 5)
		return;
	if (ip->protocol != IPPROTO_TCP && ip->protocol != IPPROTO_UDP)
		return;
	if (ip_is_fragment(ip))
		return;

	skb_set_hash(skb, hash_32((__force u32)(ip->saddr ^ ip->daddr ^ *ports), 32),
		     PKT_HASH_TYPE_L4);
}

static void mac_receive_packet(struct fusiv_eth *eth, void *buffer)
{
	struct net_device *netdev = eth->netdev;
	struct net_device_stats *stats = &netdev->stats;
	u16 flags2, start, end, length;
	struct sk_buff *skb;
	void *replacement;

	/* Discard the lines the CPU may hold, the processor wrote the buffer. */
	dma_cache_inv((unsigned long)buffer, HEADER_BYTES);
	flags2 = be16_to_cpup(buffer + BUFFER_FLAGS2_OFFSET);
	start = be16_to_cpup(buffer + BUFFER_START_OFFSET);
	end = be16_to_cpup(buffer + BUFFER_END_OFFSET);
	length = end - start;

	/* A sent packet that comes back carries no new data. */
	if (flags2 & BUFFER_FLAG_TX)
		goto give_back;

	if (end <= start || end > BUFFER_BYTES - SKB_DATA_ALIGN(sizeof(struct skb_shared_info))) {
		stats->rx_errors++;
		goto give_back;
	}

	replacement = napi_alloc_frag_align(BUFFER_BYTES, SMP_CACHE_BYTES);
	if (!replacement) {
		stats->rx_dropped++;
		goto give_back;
	}

	dma_cache_inv((unsigned long)buffer + start, length);
	skb = napi_build_skb(buffer, BUFFER_BYTES);
	if (!skb) {
		skb_free_frag(replacement);
		stats->rx_dropped++;
		goto give_back;
	}
	bmu_give_buffer(eth, replacement);

	skb_reserve(skb, start);
	skb_put(skb, length);
	stats->rx_packets++;
	stats->rx_bytes += length;
	set_flow_hash(skb);
	if (fusiv_fast_forward(skb, netdev))
		return;

	skb->protocol = eth_type_trans(skb, netdev);
	/* No GRO: the box forwards, and Wi-Fi would have to split the packets again. */
	netif_receive_skb(skb);
	return;

give_back:
	bmu_give_buffer(eth, buffer);
}

/*
 * The processor asks how many packets it may send from its queues, and sends
 * nothing until it gets an answer. All queues get the same fixed quota.
 */
static void mac_answer_quota_request(struct fusiv_eth *eth)
{
	u32 quotas = eth->mac_config.bus + QUOTA_OFFSET;

	__raw_writew(quotas >> 16, eth->mac + AP_CONFIG_HIGH);
	__raw_writew(quotas & 0xffff, eth->mac + AP_CONFIG_LOW);
	__raw_writew(CONFIG_EVENT_QUOTA, eth->mac + AP_CONFIG_EVENT);
	__raw_writel(AP_QUOTA_ANSWER_DONE, eth->mac + AP_QUOTA_ANSWER);
	eth->quota_requests++;
}

static int mac_poll(struct napi_struct *napi, int budget)
{
	struct fusiv_eth *eth = container_of(napi, struct fusiv_eth, napi);
	unsigned int queue;
	int done = 0;
	u16 status;

	status = __raw_readw(eth->mac + AP_INTERRUPT_STATUS);
	__raw_writew(status, eth->mac + AP_INTERRUPT_STATUS);
	if (status & STATUS_QUOTA_REQUEST)
		mac_answer_quota_request(eth);

	for (queue = 0; queue < QUEUE_COUNT; queue++) {
		u16 __iomem *indexes = eth->shared + MAC_QUEUE_INDEXES + queue * 4;
		u16 next_read = __raw_readw(indexes + 1);
		u16 next_write = __raw_readw(indexes);

		/* Register accesses are slow, so these indexes change once per batch. */
		if (next_read == next_write)
			continue;
		while (done < budget && next_read != next_write) {
			u32 entry = READ_ONCE(eth->mac_queues[queue][next_read]);

			next_read = (next_read + 1) % QUEUE_ENTRIES;
			if (entry)
				mac_receive_packet(eth, phys_to_virt(entry & PHYSICAL_ADDRESS_MASK));
			done++;
		}
		__raw_writew(next_read, indexes + 1);
	}

	if (done < budget && napi_complete_done(napi, done))
		__raw_writew(MAC_INTERRUPT_MASK, eth->mac + AP_INTERRUPT_MASK);
	return done;
}

static irqreturn_t mac_interrupt(int irq, void *data)
{
	struct fusiv_eth *eth = data;

	__raw_writew(MAC_ALL_INTERRUPTS_MASKED, eth->mac + AP_INTERRUPT_MASK);
	napi_schedule(&eth->napi);
	return IRQ_HANDLED;
}

static int fusiv_eth_open(struct net_device *netdev)
{
	struct fusiv_eth *eth = netdev_priv(netdev);
	int ret;

	if (!eth->started) {
		memset_io(eth->shared, 0, MAC_QUEUE_INDEXES * 2);

		ret = bmu_start(eth);
		if (ret)
			return ret;
		netdev_info(netdev, "bmu: gave it %u buffers\n", eth->buffers_given);
		mod_timer(&eth->buffer_service, jiffies + BUFFER_SERVICE_INTERVAL);

		mac_setup(eth);
		ret = mac_processor_start(eth);
		if (ret)
			return ret;
		eth->started = true;
		mod_timer(&eth->monitor, jiffies + MONITOR_INTERVAL);
	}

	ret = request_irq(eth->irq, mac_interrupt, 0, netdev->name, eth);
	if (ret)
		return ret;

	napi_enable(&eth->napi);
	eth->receiving = true;
	__raw_writew(MAC_INTERRUPT_MASK, eth->mac + AP_INTERRUPT_MASK);
	netif_carrier_on(netdev);
	netif_start_queue(netdev);
	return 0;
}

static int fusiv_eth_stop(struct net_device *netdev)
{
	struct fusiv_eth *eth = netdev_priv(netdev);

	netif_stop_queue(netdev);
	netif_carrier_off(netdev);
	eth->receiving = false;
	__raw_writew(MAC_ALL_INTERRUPTS_MASKED, eth->mac + AP_INTERRUPT_MASK);
	napi_disable(&eth->napi);
	free_irq(eth->irq, eth);
	return 0;
}

/* Copies the packet into a buffer of the pool and hands it to the processor. */
static netdev_tx_t fusiv_eth_xmit(struct sk_buff *skb, struct net_device *netdev)
{
	struct fusiv_eth *eth = netdev_priv(netdev);
	struct net_device_stats *stats = &netdev->stats;
	unsigned int length = skb->len;
	void *buffer;
	u32 address;

	if ((__raw_readw(eth->mac + AP_QUEUE_STATUS) & TX_QUEUE_LEVEL_MASK) >= TX_QUEUE_LEVEL_LIMIT ||
	    length > BUFFER_BYTES - TX_DATA_START) {
		stats->tx_dropped++;
		goto done;
	}

	address = __raw_readl(eth->bmu + BMU_TAKE_BUFFER) & PHYSICAL_ADDRESS_MASK;
	if (!address) {
		stats->tx_dropped++;
		goto done;
	}

	buffer = phys_to_virt(address);
	memset(buffer, 0, HEADER_BYTES);
	*(__be16 *)(buffer + BUFFER_FLAGS2_OFFSET) = cpu_to_be16(BUFFER_FLAG_TX);
	*(__be16 *)(buffer + BUFFER_START_OFFSET) = cpu_to_be16(TX_DATA_START);
	*(__be16 *)(buffer + BUFFER_END_OFFSET) = cpu_to_be16(TX_DATA_START + length);
	*(__be16 *)(buffer + BUFFER_LENGTH_OFFSET) = cpu_to_be16(length);
	/* The first egress port is the input queue of the MAC processor itself. */
	*(__be32 *)(buffer + BUFFER_EGRESS_OFFSET) = cpu_to_be32(MAC1_BASE + AP_INPUT_FIFO);
	skb_copy_bits(skb, 0, buffer + TX_DATA_START, length);
	dma_cache_wback_inv((unsigned long)buffer, TX_DATA_START + length);

	__raw_writel(address, eth->mac + AP_INPUT_FIFO);
	stats->tx_packets++;
	stats->tx_bytes += length;

done:
	dev_kfree_skb_any(skb);
	return NETDEV_TX_OK;
}

static const struct net_device_ops fusiv_eth_netdev_ops = {
	.ndo_open = fusiv_eth_open,
	.ndo_stop = fusiv_eth_stop,
	.ndo_validate_addr = eth_validate_addr,
	.ndo_set_mac_address = eth_mac_addr,
	.ndo_start_xmit = fusiv_eth_xmit,
};

static int fusiv_eth_probe(struct platform_device *pdev)
{
	struct net_device *netdev;
	struct fusiv_eth *eth;
	int ret;

	ret = dma_set_mask_and_coherent(&pdev->dev, DMA_BIT_MASK(32));
	if (ret)
		return ret;

	netdev = devm_alloc_etherdev(&pdev->dev, sizeof(*eth));
	if (!netdev)
		return -ENOMEM;
	SET_NETDEV_DEV(netdev, &pdev->dev);
	strscpy(netdev->name, "lan0", sizeof(netdev->name));
	netdev->netdev_ops = &fusiv_eth_netdev_ops;
	/* The driver never stops its queue, so a queue in front of it would stay empty. */
	netdev->priv_flags |= IFF_NO_QUEUE;
	eth_hw_addr_random(netdev);

	eth = netdev_priv(netdev);
	eth->dev = &pdev->dev;
	eth->netdev = netdev;
	eth->irq = platform_get_irq(pdev, 0);
	if (eth->irq < 0)
		return eth->irq;
	netif_napi_add_weight(netdev, &eth->napi, mac_poll, NAPI_BUDGET);
	eth->bmu = ioremap(BMU_BASE, 0x10000);
	eth->mac = ioremap(MAC1_BASE, 0x10000);
	eth->shared = ioremap(SHARED_MEMORY_BASE, 0x8000);
	eth->mac_registers = ioremap(MAC_REGISTERS, MAC_REGISTERS_BYTES);
	if (!eth->bmu || !eth->mac || !eth->shared || !eth->mac_registers)
		return -ENOMEM;
	timer_setup(&eth->monitor, monitor_tick, 0);
	timer_setup(&eth->buffer_service, buffer_service_tick, 0);

	return devm_register_netdev(&pdev->dev, netdev);
}

static const struct of_device_id fusiv_eth_of_match[] = {
	{ .compatible = "ikanos,fusiv-eth" },
	{ }
};

static struct platform_driver fusiv_eth_driver = {
	.probe = fusiv_eth_probe,
	.driver = {
		.name = "fusiv-eth",
		.of_match_table = fusiv_eth_of_match,
	},
};
builtin_platform_driver(fusiv_eth_driver);
