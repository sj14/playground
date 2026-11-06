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
#include <asm/irq.h>
#include <asm/prom.h>
#include <asm/reboot.h>
#include <asm/setup.h>
#include <asm/time.h>

#include "bringup.h"

#define SYSCONF_REBOOT_CTRL	CKSEG1ADDR(0x19000000)

/* UART2 is the console. Its registers are bytes spaced 4 bytes apart. */
#define UART2_BASE		CKSEG1ADDR(0x190a0000)
#define UART2_REG(reg)		(UART2_BASE + ((reg) << 2))
#define UART_READY_POLLS	10000

/* The DSL coprocessor's memory, not usable by Linux on this board. */
#define BME_START		0x06000000
#define BME_SIZE		SZ_8M

#define CPU_FREQUENCY_HZ	500000000
#define DEFAULT_MEMSIZE		SZ_128M

const char *get_system_type(void)
{
	return "Ikanos Fusiv Vx180";
}

/*
 * A UART that never gets ready must not stop the boot, because without a
 * console that hang would be invisible. Give up on it after the first timeout.
 */
void prom_putchar(char c)
{
	static bool uart_failed;
	int polls;

	bringup_log_char(c);

	if (uart_failed)
		return;

	for (polls = 0; polls < UART_READY_POLLS; polls++) {
		if (__raw_readb((void __iomem *)UART2_REG(UART_LSR)) & UART_LSR_THRE) {
			__raw_writeb(c, (void __iomem *)UART2_REG(UART_TX));
			return;
		}
	}

	uart_failed = true;
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
	bringup_set_stage(STAGE_RESTART);
	__raw_writel(1, (void __iomem *)SYSCONF_REBOOT_CTRL);

	while (true)
		cpu_relax();
}

void __init prom_init(void)
{
	bringup_init();

	init_cmdline();
	init_memory();
}

void __init prom_free_prom_memory(void)
{
}

void __init plat_mem_setup(void)
{
	bringup_set_stage(STAGE_MEM_SETUP);
	_machine_restart = fusiv_restart;

	__dt_setup_arch(get_fdt());
}

void __init device_tree_init(void)
{
	unflatten_and_copy_device_tree();
}

void __init plat_time_init(void)
{
	bringup_set_stage(STAGE_TIME_INIT);

	/* The CP0 counter runs at half the CPU clock. */
	mips_hpt_frequency = CPU_FREQUENCY_HZ / 2;

	/* The Vx180 does not report the timer line in CP0 IntCtl. */
	cp0_compare_irq = CP0_LEGACY_COMPARE_IRQ;
}
