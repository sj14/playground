// SPDX-License-Identifier: GPL-2.0-only
/*
 * PCI host controller of the Ikanos Fusiv Vx180, based on AVM's GPL kernel
 * (arch/mips/pci/pci-fusiv.c). On the FRITZ!Box 7390 the two Wi-Fi chips are
 * the only devices, in slots 1 and 2.
 */

#include <linux/delay.h>
#include <linux/init.h>
#include <linux/io.h>
#include <linux/kernel.h>
#include <linux/pci.h>
#include <linux/spinlock.h>

#include "../fusiv/fusiv.h"

#define PCI_REGS		CKSEG1ADDR(0x19170000)
#define PCI_CRP_CTL		0x00
#define PCI_CRP_WDAT		0x04
#define PCI_NPC_ADDR		0x0c
#define PCI_NPC_CTL		0x10
#define PCI_NPC_WDAT		0x14
#define PCI_NPC_RDAT		0x18
#define PCI_ERROR		0x1c
#define PCI_CONTROL		0x38
#define PCI_MBAR_CONF		0x44
#define PCI_IMBAR1_MASK		0x48
#define PCI_IMEM1_TRANS		0x4c
#define PCI_IMBAR2_MASK		0x50
#define PCI_IMEM2_TRANS		0x54
#define PCI_ID_REG		0x68
#define PCI_REV_REG		0x6c
#define PCI_SVID_REG		0x70
#define PCI_MBAP1		0x7c
#define PCI_MBAP2		0x80
#define PCI_MBAP3		0x84
#define PCI_MBAP4		0x88

#define PCI_CONTROL_ENABLE	BIT(0)
#define PCI_CONTROL_INB_TRANS	BIT(1)
#define PCI_CONTROL_INTA_EN	BIT(4)

/* Master abort, target abort and AHB error. */
#define PCI_ERROR_ABORT_MASK	0x103

#define PCI_CMD_CONFIG_READ	0xa
#define PCI_CMD_CONFIG_WRITE	0xb
#define PCI_CRP_CMD_SHIFT	16

#define PCI_RESET_CONTROL	CKSEG1ADDR(0x19050700)

#define PCI_MEM_BASE		0x1a000000
#define PCI_MEM_END		0x1affffff

/* The local identity of the controller, as AVM sets it. */
#define PCI_LOCAL_ID		0x55555555
#define PCI_LOCAL_CLASS_REV	0x00000201

#define PCI_MAX_SLOT		4
/* Address bit 21 selects slot 1, bit 22 slot 2 and so on. */
#define PCI_IDSEL_BIT(slot)	BIT((slot) + 20)

#define PCI_INTERRUPT_SOURCE	25

/* The chips can answer the first access with a retry while they load their EEPROM. */
#define VENDOR_READ_ATTEMPTS	5
#define VENDOR_READ_DELAY_MS	100

static DEFINE_RAW_SPINLOCK(fusiv_pci_lock);

static u32 pci_reg_read(unsigned int offset)
{
	return __raw_readl((void __iomem *)(PCI_REGS + offset));
}

static void pci_reg_write(unsigned int offset, u32 value)
{
	__raw_writel(value, (void __iomem *)(PCI_REGS + offset));
}

static int config_transfer(unsigned int cmd, struct pci_bus *bus,
			   unsigned int devfn, int where, u32 *value)
{
	unsigned long flags;
	int result = PCIBIOS_SUCCESSFUL;

	if (bus->number != 0 || PCI_SLOT(devfn) == 0 || PCI_SLOT(devfn) > PCI_MAX_SLOT)
		return PCIBIOS_DEVICE_NOT_FOUND;

	raw_spin_lock_irqsave(&fusiv_pci_lock, flags);

	pci_reg_write(PCI_NPC_ADDR, PCI_IDSEL_BIT(PCI_SLOT(devfn)) |
				    (PCI_FUNC(devfn) << 8) | (where & ~3));
	pci_reg_write(PCI_NPC_CTL, cmd);

	if (cmd == PCI_CMD_CONFIG_WRITE)
		pci_reg_write(PCI_NPC_WDAT, *value);
	else
		*value = pci_reg_read(PCI_NPC_RDAT);

	if (pci_reg_read(PCI_ERROR) & PCI_ERROR_ABORT_MASK) {
		pci_reg_write(PCI_ERROR, 0xffffffff);
		result = PCIBIOS_DEVICE_NOT_FOUND;
	}

	raw_spin_unlock_irqrestore(&fusiv_pci_lock, flags);

	return result;
}

static bool vendor_id_is_invalid(u32 value)
{
	return (value & 0xffff) == 0xffff || (value & 0xffff) == 0 ||
	       (value >> 16) == 0xffff || (value >> 16) == 0;
}

static int fusiv_pci_read(struct pci_bus *bus, unsigned int devfn, int where,
			  int size, u32 *value)
{
	unsigned int shift = (where & 3) * 8;
	int attempts = where == PCI_VENDOR_ID ? VENDOR_READ_ATTEMPTS : 1;
	u32 data = 0;
	int result;

	while (true) {
		result = config_transfer(PCI_CMD_CONFIG_READ, bus, devfn, where, &data);
		attempts--;
		if (result != PCIBIOS_SUCCESSFUL || attempts == 0 ||
		    !vendor_id_is_invalid(data))
			break;
		mdelay(VENDOR_READ_DELAY_MS);
	}

	if (result != PCIBIOS_SUCCESSFUL)
		return result;

	if (size == 1)
		*value = (data >> shift) & 0xff;
	else if (size == 2)
		*value = (data >> shift) & 0xffff;
	else
		*value = data;

	return PCIBIOS_SUCCESSFUL;
}

static int fusiv_pci_write(struct pci_bus *bus, unsigned int devfn, int where,
			   int size, u32 value)
{
	unsigned int shift = (where & 3) * 8;
	u32 data = 0;
	int result;

	result = config_transfer(PCI_CMD_CONFIG_READ, bus, devfn, where, &data);
	if (result != PCIBIOS_SUCCESSFUL)
		return result;

	if (size == 1)
		data = (data & ~(0xff << shift)) | (value << shift);
	else if (size == 2)
		data = (data & ~(0xffff << shift)) | (value << shift);
	else
		data = value;

	return config_transfer(PCI_CMD_CONFIG_WRITE, bus, devfn, where, &data);
}

static struct pci_ops fusiv_pci_ops = {
	.read	= fusiv_pci_read,
	.write	= fusiv_pci_write,
};

static struct resource fusiv_pci_mem_resource = {
	.name	= "FUSIV PCI MEM",
	.start	= PCI_MEM_BASE,
	.end	= PCI_MEM_END,
	.flags	= IORESOURCE_MEM,
};

/* No device on this board uses I/O ports, but the controller needs a range. */
static struct resource fusiv_pci_io_resource = {
	.name	= "FUSIV PCI IO",
	.start	= 0x00000100,
	.end	= 0x0000ffff,
	.flags	= IORESOURCE_IO,
};

static struct pci_controller fusiv_pci_controller = {
	.mem_resource	= &fusiv_pci_mem_resource,
	.io_resource	= &fusiv_pci_io_resource,
	.pci_ops	= &fusiv_pci_ops,
};

static void __init fusiv_pci_reset(void)
{
	void __iomem *reset = (void __iomem *)PCI_RESET_CONTROL;

	__raw_writel(0x1, reset);
	mdelay(1);
	__raw_writel(0x3, reset);
	mdelay(5);
	__raw_writel(0x1, reset);
	mdelay(5);
}

static int __init fusiv_pci_init(void)
{
	pr_info("fusiv-pci: starting\n");

	fusiv_pci_reset();

	pci_reg_write(PCI_ID_REG, PCI_LOCAL_ID);
	pci_reg_write(PCI_REV_REG, PCI_LOCAL_CLASS_REV);
	pci_reg_write(PCI_SVID_REG, PCI_LOCAL_ID);
	pci_reg_write(PCI_CONTROL, PCI_CONTROL_ENABLE | PCI_CONTROL_INTA_EN |
				   PCI_CONTROL_INB_TRANS);

	/* Devices reach the system memory at its physical address. */
	pci_reg_write(PCI_MBAR_CONF, 0x11);
	pci_reg_write(PCI_IMBAR1_MASK, 0xf0000000);
	pci_reg_write(PCI_IMEM1_TRANS, 0x00000000);
	pci_reg_write(PCI_IMBAR2_MASK, 0xff000000);
	pci_reg_write(PCI_IMEM2_TRANS, 0x19000000);

	/* Make the controller a bus master and clear its status bits. */
	pci_reg_write(PCI_CRP_CTL, PCI_COMMAND | (PCI_CMD_CONFIG_WRITE << PCI_CRP_CMD_SHIFT));
	pci_reg_write(PCI_CRP_WDAT, PCI_COMMAND_SERR | PCI_COMMAND_PARITY |
				    PCI_COMMAND_MEMORY | PCI_COMMAND_MASTER |
				    (0xffff << 16));

	pci_reg_write(PCI_MBAP1, PCI_MEM_BASE);
	pci_reg_write(PCI_MBAP2, PCI_MEM_BASE + 0x01000000);
	pci_reg_write(PCI_MBAP3, PCI_MEM_BASE + 0x02000000);
	pci_reg_write(PCI_MBAP4, PCI_MEM_BASE + 0x03000000);

	mdelay(20);

	register_pci_controller(&fusiv_pci_controller);

	return 0;
}
arch_initcall(fusiv_pci_init);

int pcibios_map_irq(const struct pci_dev *dev, u8 slot, u8 pin)
{
	if (slot == 0 || slot > PCI_MAX_SLOT)
		return -1;

	return fusiv_ipc_map_irq(PCI_INTERRUPT_SOURCE);
}

int pcibios_plat_dev_init(struct pci_dev *dev)
{
	return 0;
}
