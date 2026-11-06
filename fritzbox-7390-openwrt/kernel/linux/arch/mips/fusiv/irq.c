// SPDX-License-Identifier: GPL-2.0-only
/*
 * Interrupt controller of the Ikanos Fusiv Vx180.
 *
 * Each source has a 4-bit priority that selects a CPU line: priority N raises
 * CPU interrupt N + 2. All sources are routed to the lowest priority and
 * handled through one cascade, except for the CP0 timer, which gets its own
 * line (priority 5, CPU interrupt 7).
 */

#include <linux/init.h>
#include <linux/interrupt.h>
#include <linux/io.h>
#include <linux/irq.h>
#include <linux/irqchip.h>
#include <linux/irqchip/chained_irq.h>
#include <linux/irqdomain.h>
#include <linux/of_address.h>
#include <linux/of_irq.h>
#include <linux/spinlock.h>

#define IPC_PRIO(irq)		(0x100 + ((irq) / 4) * 4)
#define IPC_PRIO_SHIFT(irq)	(((irq) % 4) * 8)
#define IPC_PRIO_MASK		0xff
#define IPC_PRIO_ROUTED		0x00
#define IPC_PRIO_TIMER		0x05
#define IPC_PRIO_UNROUTED	0x0f
#define IPC_PRIO_ALL_UNROUTED	0x0f0f0f0f
#define IPC_PRIO_REGS		10

/* Status of the sources routed to priority 0, one word per 32 sources. */
#define IPC_STAT_LOW		0x128
#define IPC_STAT_HIGH		0x12c

#define IPC_NUM_IRQS		38
#define IPC_TIMER_IRQ		3

static void __iomem *ipc_base;
static struct irq_domain *ipc_domain;
static DEFINE_RAW_SPINLOCK(ipc_lock);

static void ipc_set_prio(unsigned int irq, u32 prio)
{
	unsigned long flags;
	u32 value;

	raw_spin_lock_irqsave(&ipc_lock, flags);
	value = __raw_readl(ipc_base + IPC_PRIO(irq));
	value &= ~(IPC_PRIO_MASK << IPC_PRIO_SHIFT(irq));
	value |= prio << IPC_PRIO_SHIFT(irq);
	__raw_writel(value, ipc_base + IPC_PRIO(irq));
	raw_spin_unlock_irqrestore(&ipc_lock, flags);
}

static void ipc_irq_mask(struct irq_data *data)
{
	ipc_set_prio(data->hwirq, IPC_PRIO_UNROUTED);
}

static void ipc_irq_unmask(struct irq_data *data)
{
	ipc_set_prio(data->hwirq, IPC_PRIO_ROUTED);
}

static struct irq_chip ipc_chip = {
	.name		= "fusiv-ipc",
	.irq_mask	= ipc_irq_mask,
	.irq_unmask	= ipc_irq_unmask,
};

static void ipc_handle_status(u32 status, unsigned int first_irq)
{
	while (status) {
		unsigned int bit = __ffs(status);

		generic_handle_domain_irq(ipc_domain, first_irq + bit);
		status &= ~BIT(bit);
	}
}

static void ipc_cascade(struct irq_desc *desc)
{
	struct irq_chip *chip = irq_desc_get_chip(desc);

	chained_irq_enter(chip, desc);
	ipc_handle_status(__raw_readl(ipc_base + IPC_STAT_LOW), 0);
	ipc_handle_status(__raw_readl(ipc_base + IPC_STAT_HIGH), 32);
	chained_irq_exit(chip, desc);
}

static int ipc_domain_map(struct irq_domain *domain, unsigned int irq,
			  irq_hw_number_t hwirq)
{
	irq_set_chip_and_handler(irq, &ipc_chip, handle_level_irq);

	return 0;
}

static const struct irq_domain_ops ipc_domain_ops = {
	.map	= ipc_domain_map,
	.xlate	= irq_domain_xlate_onecell,
};

static int __init ipc_of_init(struct device_node *node,
			      struct device_node *parent)
{
	unsigned int parent_irq;
	int i;

	ipc_base = of_iomap(node, 0);
	if (!ipc_base)
		return -ENOMEM;

	for (i = 0; i < IPC_PRIO_REGS; i++)
		__raw_writel(IPC_PRIO_ALL_UNROUTED, ipc_base + 0x100 + i * 4);

	ipc_domain = irq_domain_create_linear(of_fwnode_handle(node), IPC_NUM_IRQS,
					      &ipc_domain_ops, NULL);
	if (!ipc_domain)
		return -ENOMEM;

	parent_irq = irq_of_parse_and_map(node, 0);
	if (!parent_irq)
		return -EINVAL;

	irq_set_chained_handler(parent_irq, ipc_cascade);

	/* Without this route the CP0 timer never reaches the CPU. */
	ipc_set_prio(IPC_TIMER_IRQ, IPC_PRIO_TIMER);

	return 0;
}
IRQCHIP_DECLARE(fusiv_ipc, "ikanos,vx180-ipc", ipc_of_init);

void __init arch_init_irq(void)
{
	irqchip_init();
}
