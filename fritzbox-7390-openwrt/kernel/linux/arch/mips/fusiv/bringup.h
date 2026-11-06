/* SPDX-License-Identifier: GPL-2.0-only */
#ifndef _FUSIV_BRINGUP_H
#define _FUSIV_BRINGUP_H

/* The AVM reboot status text, which survives a software reset. */
#define FUSIV_MAILBOX_PHYS	0x00fffe00
#define FUSIV_MAILBOX_SIZE	512

/* How far the boot got. The value is stored in the RAM log header. */
enum bringup_stage {
	STAGE_PROM_INIT = 1,
	STAGE_MEM_SETUP,
	STAGE_IRQ_INIT,
	STAGE_TIME_INIT,
	STAGE_TIMER_RUNNING,
	STAGE_RESTART,
};

void bringup_init(void);
void bringup_set_stage(enum bringup_stage stage);
void bringup_log_char(char c);

#endif
