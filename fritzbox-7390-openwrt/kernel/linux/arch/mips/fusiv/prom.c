// SPDX-License-Identifier: GPL-2.0-only
/*
 * Boot support for the Ikanos Fusiv Vx180 as used in the AVM FRITZ!Box 7390.
 *
 * Register addresses come from AVM's GPL kernel (arch/mips/fusiv/fusiv_mips32
 * and arch/mips/include/asm/mach-fusiv/vx180.h).
 */

#include <linux/init.h>
#include <linux/io.h>
#include <linux/kernel.h>
#include <linux/memblock.h>
#include <linux/of_fdt.h>
#include <linux/serial_reg.h>
#include <linux/sizes.h>
#include <linux/string.h>

#include <asm/bootinfo.h>
#include <asm/fw/fw.h>
#include <asm/prom.h>
#include <asm/reboot.h>
#include <asm/setup.h>
#include <asm/time.h>

#define SYSCONF_BASE		CKSEG1ADDR(0x19000000)
#define SYSCONF_REBOOT_CTRL	(SYSCONF_BASE + 0x00)
#define SYSCONF_WDT_CTRL	(SYSCONF_BASE + 0x08)
#define WDT_CTRL_DISABLE	BIT(0)
#define WDT_CTRL_RELOAD_MAX	(0xfffc << 16)

/* UART2 is the console. Its registers are bytes spaced 4 bytes apart. */
#define UART2_BASE		CKSEG1ADDR(0x190a0000)
#define UART2_REG(reg)		(UART2_BASE + ((reg) << 2))

/* The DSL coprocessor's memory, not usable by Linux on this board. */
#define BME_START		0x06000000
#define BME_SIZE		SZ_8M

#define CPU_FREQUENCY_HZ	500000000
#define DEFAULT_MEMSIZE		SZ_128M

const char *get_system_type(void)
{
	return "Ikanos Fusiv Vx180";
}

void prom_putchar(char c)
{
	while (!(__raw_readb((void __iomem *)UART2_REG(UART_LSR)) & UART_LSR_THRE))
		;

	__raw_writeb(c, (void __iomem *)UART2_REG(UART_TX));
}

/* EVA passes the environment as an array of name and value pointers. */
static const char * __init eva_getenv(const char *name)
{
	char **env = (char **)fw_arg2;

	if (fw_arg2 < CKSEG0)
		return NULL;

	for (; env[0]; env += 2) {
		if (!strcmp(env[0], name))
			return env[1];
	}

	return NULL;
}

static void __init init_cmdline(void)
{
	int argc = fw_arg0;
	char **argv = (char **)fw_arg1;
	int i;

	if (fw_arg1 < CKSEG0)
		return;

	for (i = 1; i < argc; i++) {
		strlcat(arcs_cmdline, argv[i], COMMAND_LINE_SIZE);
		strlcat(arcs_cmdline, " ", COMMAND_LINE_SIZE);
	}
}

static void __init init_memory(void)
{
	const char *memsize_env = eva_getenv("memsize");
	unsigned long memsize = DEFAULT_MEMSIZE;

	if (memsize_env)
		memsize = simple_strtoul(memsize_env, NULL, 0);

	memblock_add(0, min_t(unsigned long, memsize, BME_START));

	if (memsize > BME_START + BME_SIZE)
		memblock_add(BME_START + BME_SIZE, memsize - BME_START - BME_SIZE);
}

static void fusiv_restart(char *command)
{
	__raw_writel(1, (void __iomem *)SYSCONF_REBOOT_CTRL);

	while (true)
		cpu_relax();
}

void __init prom_init(void)
{
	/* The same write AVM's driver uses to stop the watchdog. */
	__raw_writel(WDT_CTRL_DISABLE | WDT_CTRL_RELOAD_MAX,
		     (void __iomem *)SYSCONF_WDT_CTRL);

	init_cmdline();
	init_memory();
}

void __init prom_free_prom_memory(void)
{
}

void __init plat_mem_setup(void)
{
	_machine_restart = fusiv_restart;

	__dt_setup_arch(get_fdt());
}

void __init device_tree_init(void)
{
	unflatten_and_copy_device_tree();
}

void __init plat_time_init(void)
{
	/* The CP0 counter runs at half the CPU clock. */
	mips_hpt_frequency = CPU_FREQUENCY_HZ / 2;

	/* The Vx180 does not report the timer line in CP0 IntCtl. */
	cp0_compare_irq = CP0_LEGACY_COMPARE_IRQ;
}
