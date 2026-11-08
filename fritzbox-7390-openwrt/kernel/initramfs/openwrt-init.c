// /init of the OpenWrt image. It prepares the hardware that has no kernel driver
// yet, then hands over to OpenWrt's init the same way as OpenWrt's own initramfs.

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <unistd.h>

#include "board.h"

// 0 keeps the box running, for an installed image.
#ifndef TEST_DURATION_S
#define TEST_DURATION_S 600
#endif

// OpenWrt's log of a test image, see openwrt/uci-defaults-test.
static const char OPENWRT_LOG[] = "/tmp/openwrt.log";
// Adds the state of the test image to that log, see openwrt/test-diagnostics.
static const char DIAGNOSTICS[] = "/usr/sbin/test-diagnostics >> /tmp/openwrt.log 2>&1";

// Copies OpenWrt's log to the kernel log, so that it survives the restart in the
// RAM log. Kernel messages are left out, they are there already.
static void copy_openwrt_log(void) {
    char line[512];
    FILE *log = fopen(OPENWRT_LOG, "r");

    if (log == NULL) {
        log_line("test image: no OpenWrt log\n");
        return;
    }
    while (fgets(line, sizeof(line), log) != NULL) {
        if (strstr(line, " kernel: ") == NULL) {
            log_line(line);
        }
    }
    fclose(log);
}

// A test image restarts the box after TEST_DURATION_S, so that the box returns to
// the installed firmware even if OpenWrt never becomes reachable.
static void schedule_restart(void) {
    char line[96];

    if (TEST_DURATION_S == 0 || fork() != 0) {
        return;
    }
    setsid();
    sleep(TEST_DURATION_S);
    if (system(DIAGNOSTICS) != 0) {
        log_line("test image: diagnostics failed\n");
    }
    copy_openwrt_log();
    snprintf(line, sizeof(line), "test image: restarting after %d s\n", TEST_DURATION_S);
    log_line(line);
    sync();
    reboot(RB_AUTOBOOT);
    _exit(1);
}

int main(void) {
    char line[128];

    // O_CLOEXEC: OpenWrt's init opens the kernel log itself.
    kmsg_fd = open("/dev/kmsg", O_WRONLY | O_CLOEXEC);
    mount("proc", "/proc", "proc", 0, NULL);
    mount("sysfs", "/sys", "sysfs", 0, NULL);
    load_fpga();
    log_switch_identity();
    configure_switch();
    umount("/sys");
    umount("/proc");

    schedule_restart();
    setenv("INITRAMFS", "1", 1);
    execl("/sbin/init", "/sbin/init", (char *)NULL);
    snprintf(line, sizeof(line), "cannot start /sbin/init: %s\n", strerror(errno));
    log_line(line);
    return 1;
}
