// First userspace program for bring-up. It shows that user mode and the timer
// work and then restarts the box, so that the AVM firmware can read the RAM log.
// Output also goes to /dev/kmsg, because only kernel messages reach the RAM log.

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>

enum {
    TICK_MS = 5000,
    TEST_DURATION_S = 30,
};

static int kmsg_fd = -1;

static void log_line(const char *line) {
    fputs(line, stdout);
    fflush(stdout);
    if (kmsg_fd >= 0) {
        write(kmsg_fd, line, strlen(line));
    }
}

static void log_file(const char *path) {
    char line[256];
    FILE *file = fopen(path, "r");
    if (!file) {
        snprintf(line, sizeof(line), "cannot open %s: %s\n", path, strerror(errno));
        log_line(line);
        return;
    }

    snprintf(line, sizeof(line), "--- %s\n", path);
    log_line(line);
    while (fgets(line, sizeof(line), file)) {
        log_line(line);
    }
    fclose(file);
}

static long long uptime_seconds(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return now.tv_sec;
}

// Reports in the AVM reboot status that /init really ran, because this text
// survives the reset. The firmware then shows "watchdog".
static void report_init_ran(void) {
    enum { MAILBOX_PAGE = 0x00fff000, MAILBOX_OFFSET = 0xe00, PAGE_SIZE_BYTES = 4096 };
    static const char status[] =
        "(c) AVM 2013, Reboot Status is: Software-NMI-Watchdog"
        "(c) AVM 2013, Reboot Status is: Software-NMI-Watchdog"
        "(c) AVM 2013, Reboot Status is: Software-NMI-Watchdog";

    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (fd < 0) {
        log_line("cannot open /dev/mem\n");
        return;
    }

    char *page = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ | PROT_WRITE, MAP_SHARED, fd, MAILBOX_PAGE);
    if (page == MAP_FAILED) {
        log_line("cannot map the status mailbox\n");
    } else {
        volatile char *mailbox = page + MAILBOX_OFFSET;
        for (size_t i = 0; i < sizeof(status); i++) {
            mailbox[i] = status[i];
        }
        munmap(page, PAGE_SIZE_BYTES);
    }
    close(fd);
}

int main(void) {
    char line[128];
    struct utsname name;

    kmsg_fd = open("/dev/kmsg", O_WRONLY);
    report_init_ran();

    uname(&name);
    snprintf(line, sizeof(line), "Hello from userspace on %s %s\n", name.sysname, name.release);
    log_line(line);

    if (mount("proc", "/proc", "proc", 0, NULL) != 0) {
        snprintf(line, sizeof(line), "mount /proc: %s\n", strerror(errno));
        log_line(line);
    }
    log_file("/proc/cpuinfo");
    log_file("/proc/interrupts");
    log_file("/proc/iomem");
    log_file("/proc/bus/pci/devices");
    log_file("/proc/mtd");
    log_file("/proc/net/dev");

    long long start = uptime_seconds();
    struct pollfd input = {.fd = STDIN_FILENO, .events = POLLIN};
    while (uptime_seconds() - start < TEST_DURATION_S) {
        int ready = poll(&input, 1, TICK_MS);
        if (ready > 0) {
            char buffer[64];
            ssize_t length = read(STDIN_FILENO, buffer, sizeof(buffer));
            if (length > 0) {
                write(STDOUT_FILENO, buffer, length);
            }
        } else if (ready == 0) {
            snprintf(line, sizeof(line), "tick %lld s\n", uptime_seconds());
            log_line(line);
        }
    }

    log_file("/proc/interrupts");
    log_line("Test finished, restarting\n");
    reboot(RB_AUTOBOOT);

    snprintf(line, sizeof(line), "reboot failed: %s\n", strerror(errno));
    log_line(line);
    return 1;
}
