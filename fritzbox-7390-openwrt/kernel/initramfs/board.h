// Hardware setup and logging shared by the test init and the OpenWrt init.
#ifndef BOARD_H
#define BOARD_H

// /dev/kmsg, so that output reaches the kernel log and with it the RAM log.
extern int kmsg_fd;

void log_line(const char *line);
// Reports in the AVM reboot status that /init really ran.
void report_init_ran(void);
unsigned int register_at(volatile char *base, unsigned int offset);
// Loads the FPGA, which powers the USB port, if the image contains its bitstream.
void load_fpga(void);
// Releases the switch from reset and logs its identity.
void log_switch_identity(void);
// Sets up the switch's connection to the Ethernet block.
void configure_switch(void);
void log_link_speeds(void);

#endif
