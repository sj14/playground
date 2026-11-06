// SPDX-License-Identifier: GPL-2.0-only
/*
 * Debugging aids for bringing up the kernel without a serial console:
 *
 * - All kernel messages go to a RAM log, which survives a software reset.
 *   tools/fusiv-ramlog.c reads it after the box is back in the AVM firmware.
 * - The LEDs show how far the boot got.
 * - The hardware watchdog restarts the box if the kernel hangs.
 * - A kernel timer serves the watchdog and restarts the box after a deadline,
 *   so a test run always ends.
 *
 * Register usage follows AVM's GPL kernel (ikan_wdt.c, ikan_gpio.c, vx180.h).
 */

#include <linux/bits.h>
#include <linux/init.h>
#include <linux/io.h>
#include <linux/jiffies.h>
#include <linux/kernel.h>
#include <linux/printk.h>
#include <linux/reboot.h>
#include <linux/sizes.h>
#include <linux/string.h>
#include <linux/timer.h>

#include "bringup.h"

/*
 * An area of the memory AVM reserves for the DSL coprocessor. Linux does not
 * use this memory. Under the AVM firmware most of the coprocessor's memory is
 * full of data, but this part held only 23 non-zero words of 65536. Those
 * writes may still damage single spots of the log.
 */
#define RAMLOG_PHYS		0x06240000
#define RAMLOG_SIZE		SZ_256K
#define RAMLOG_BASE		CKSEG1ADDR(RAMLOG_PHYS)
#define RAMLOG_MAGIC		0x46534c47	/* "FSLG" */
#define RAMLOG_MAGIC_OFFSET	0x0
#define RAMLOG_LENGTH_OFFSET	0x4
#define RAMLOG_STAGE_OFFSET	0x8
#define RAMLOG_DATA_OFFSET	0x10
#define RAMLOG_CAPACITY		(RAMLOG_SIZE - RAMLOG_DATA_OFFSET)

/*
 * The AVM firmware reads this text after a reset and reports it in
 * /proc/sys/urlader/reboot_status. It has survived resets on this box, so it
 * shows the progress of a test run even if the RAM log is lost:
 *
 *   software  the kernel never reached prom_init
 *   poweron   prom_init was reached
 *   nmi       timer interrupts work
 */
#define STATUS_TEXT(status)	"(c) AVM 2013, Reboot Status is: " status
#define STATUS_REPEATED(status)	STATUS_TEXT(status) STATUS_TEXT(status) STATUS_TEXT(status)
#define STATUS_POWERON		STATUS_REPEATED("Power-On-Reboot")
#define STATUS_TIMER		STATUS_REPEATED("Software-NMI-Watchdog")

#define WDT_CTRL		CKSEG1ADDR(0x19000008)
#define WDT_CTRL_DISABLE	BIT(0)
#define WDT_CTRL_SERVICE	(0xa << 8)
#define WDT_CTRL_RELOAD_MAX	(0xfffc << 16)

/* The counter runs at about 10170 Hz, so 0xfffc lasts about 6.4 s. */
#define WDT_SERVICE_INTERVAL	(HZ / 10)
#define TEST_DEADLINE_SECONDS	90

/* The LEDs are GPIOs 16 to 31, active low. */
#define GPIO_BANK1_DIR		CKSEG1ADDR(0x190b0000)
#define GPIO_BANK1_CLEAR	CKSEG1ADDR(0x190b0004)
#define LED_INFO		BIT(22 - 16)
#define LED_FESTNETZ		BIT(26 - 16)
#define LED_WLAN		BIT(27 - 16)
#define LED_INTERNET		BIT(28 - 16)

static u32 ramlog_length;
static struct timer_list wdt_timer;
static unsigned long test_deadline;

static void led_on(u16 led)
{
	u16 direction = __raw_readw((void __iomem *)GPIO_BANK1_DIR);

	__raw_writew(direction | led, (void __iomem *)GPIO_BANK1_DIR);
	__raw_writew(led, (void __iomem *)GPIO_BANK1_CLEAR);
}

static void mailbox_write(const char *text)
{
	void __iomem *mailbox = (void __iomem *)CKSEG1ADDR(FUSIV_MAILBOX_PHYS);
	size_t length = strlen(text);
	size_t i;

	for (i = 0; i <= length; i++)
		__raw_writeb(text[i], mailbox + i);
}

static void ramlog_init(void)
{
	void __iomem *base = (void __iomem *)RAMLOG_BASE;

	ramlog_length = 0;
	__raw_writel(0, base + RAMLOG_LENGTH_OFFSET);
	__raw_writel(0, base + RAMLOG_STAGE_OFFSET);
	__raw_writel(RAMLOG_MAGIC, base + RAMLOG_MAGIC_OFFSET);
}

void bringup_log_char(char c)
{
	void __iomem *base = (void __iomem *)RAMLOG_BASE;

	if (ramlog_length >= RAMLOG_CAPACITY)
		return;

	__raw_writeb(c, base + RAMLOG_DATA_OFFSET + ramlog_length);
	ramlog_length++;
	__raw_writel(ramlog_length, base + RAMLOG_LENGTH_OFFSET);
}

void bringup_set_stage(enum bringup_stage stage)
{
	__raw_writel(stage, (void __iomem *)(RAMLOG_BASE + RAMLOG_STAGE_OFFSET));

	switch (stage) {
	case STAGE_PROM_INIT:
		led_on(LED_INTERNET);
		break;
	case STAGE_IRQ_INIT:
		led_on(LED_FESTNETZ);
		break;
	case STAGE_TIME_INIT:
		led_on(LED_WLAN);
		break;
	case STAGE_TIMER_RUNNING:
		led_on(LED_INFO);
		break;
	default:
		break;
	}
}

static void wdt_start(void)
{
	void __iomem *ctrl = (void __iomem *)WDT_CTRL;

	__raw_writel(WDT_CTRL_DISABLE | WDT_CTRL_RELOAD_MAX, ctrl);
	__raw_writel(WDT_CTRL_SERVICE | WDT_CTRL_RELOAD_MAX, ctrl);
}

void bringup_init(void)
{
	mailbox_write(STATUS_POWERON);
	ramlog_init();
	bringup_set_stage(STAGE_PROM_INIT);
	wdt_start();
}

static void wdt_timer_tick(struct timer_list *timer)
{
	static bool first_tick_seen;

	__raw_writel(WDT_CTRL_SERVICE, (void __iomem *)WDT_CTRL);

	if (!first_tick_seen) {
		first_tick_seen = true;
		mailbox_write(STATUS_TIMER);
		bringup_set_stage(STAGE_TIMER_RUNNING);
		pr_info("bringup: timer interrupts work\n");
	}

	if (time_after(jiffies, test_deadline)) {
		pr_info("bringup: test deadline reached, restarting\n");
		machine_restart(NULL);
	}

	mod_timer(timer, jiffies + WDT_SERVICE_INTERVAL);
}

static int __init wdt_timer_init(void)
{
	test_deadline = jiffies + TEST_DEADLINE_SECONDS * HZ;
	timer_setup(&wdt_timer, wdt_timer_tick, 0);
	mod_timer(&wdt_timer, jiffies + WDT_SERVICE_INTERVAL);

	return 0;
}
early_initcall(wdt_timer_init);
