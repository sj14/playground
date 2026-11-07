// SPDX-License-Identifier: GPL-2.0-only
/*
 * Prepares the USB host controller of the Ikanos Fusiv Vx180 for the generic
 * EHCI driver. The sequence and its values follow AVM's GPL kernel
 * (drivers/usb/host/ehci-ikf68xx.c).
 */

#include <linux/bits.h>
#include <linux/delay.h>
#include <linux/init.h>
#include <linux/io.h>

#define SYSTEM_CONTROL		CKSEG1ADDR(0x19000088)
#define SYSTEM_CONTROL_ENDIAN	(BIT(30) | BIT(29))
#define CLK_GATE_CONTROL	CKSEG1ADDR(0x190000cc)
#define CLK_GATE_USB_HOST	BIT(13)
#define USB_PHY_CONTROL		CKSEG1ADDR(0x190000dc)
#define USB_PHY_CLOCK_ON	0x01
#define USB_PHY_RESET_ALL	0x31
#define USB_PHY_RESET		0x05
#define USB_PHY_RUN		0x07
#define EHCI_FRAME_LENGTH	CKSEG1ADDR(0x19230090)
#define EHCI_FRAME_LENGTH_VALUE	0x00001d4d
#define USB_SETTLE_MS		1
#define USB_PHY_RESET_FIRST_MS	1
#define USB_PHY_RESET_SECOND_MS	5

static void __init usb_phy_set(u32 value)
{
	__raw_writel(value, (void __iomem *)USB_PHY_CONTROL);
	mdelay(USB_SETTLE_MS);
}

static void __init set_bits(unsigned long address, u32 bits)
{
	void __iomem *reg = (void __iomem *)address;

	__raw_writel(__raw_readl(reg) | bits, reg);
}

/* AVM's kernel runs this twice, for EHCI and then for OHCI, the second time with a longer wait. */
static void __init usb_phy_reset_sequence(unsigned int reset_wait_ms)
{
	usb_phy_set(USB_PHY_CLOCK_ON);
	__raw_writel(USB_PHY_RESET_ALL, (void __iomem *)USB_PHY_CONTROL);
	mdelay(reset_wait_ms);
	usb_phy_set(USB_PHY_RESET);
	usb_phy_set(USB_PHY_CLOCK_ON);
	usb_phy_set(USB_PHY_RUN);
}

static int __init fusiv_usb_init(void)
{
	usb_phy_reset_sequence(USB_PHY_RESET_FIRST_MS);
	usb_phy_reset_sequence(USB_PHY_RESET_SECOND_MS);

	set_bits(CLK_GATE_CONTROL, CLK_GATE_USB_HOST);
	set_bits(SYSTEM_CONTROL, SYSTEM_CONTROL_ENDIAN);
	mdelay(USB_SETTLE_MS);

	__raw_writel(EHCI_FRAME_LENGTH_VALUE, (void __iomem *)EHCI_FRAME_LENGTH);

	return 0;
}
arch_initcall(fusiv_usb_init);
