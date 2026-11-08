// First userspace program for bring-up. It shows that user mode and the timer
// work, starts a Wi-Fi access point and then restarts the box, so that the AVM
// firmware can read the RAM log. Output also goes to /dev/kmsg, because only
// kernel messages reach the RAM log.

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/sockios.h>
#include <net/if.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include "board.h"

enum {
    SEND_PORT = 5001,
    RECEIVE_PORT = 5002,
    TRANSFER_BUFFER_SIZE = 16384,
    WIRED_WAIT_S = 20,
};

static const char AP_NETMASK[] = "255.255.255.0";
// The LAN port replaces the USB adapter in the bridge. Both at once on the same
// switch would form a loop.
static const char WIRED_INTERFACE[] = "lan0";
static const char BRIDGE_INTERFACE[] = "br0";
static const char BRIDGE_ADDRESS[] = "192.168.0.250";

#ifndef TICK_S
#define TICK_S 15
#endif

#ifndef STATUS_INTERVAL_S
#define STATUS_INTERVAL_S 120
#endif

// 0 keeps the box running, for an installed image.
#ifndef TEST_DURATION_S
#define TEST_DURATION_S 180
#endif



/* The first line of /proc/stat: CPU time spent in each state since boot. */
static void log_cpu_usage(void) {
    char line[256];
    FILE *file = fopen("/proc/stat", "r");
    if (!file) {
        return;
    }
    if (fgets(line, sizeof(line), file)) {
        log_line(line);
    }
    fclose(file);
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


// Logs the PCI bridge control register. It reads zero when the bridge hangs.
static void log_bridge_state(void) {
    enum { BRIDGE_PAGE = 0x19170000, CONTROL_OFFSET = 0x38, PAGE_SIZE_BYTES = 4096 };
    char line[64];

    int fd = open("/dev/mem", O_RDONLY | O_SYNC);
    if (fd < 0) {
        return;
    }
    void *page = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ, MAP_SHARED, fd, BRIDGE_PAGE);
    if (page != MAP_FAILED) {
        volatile unsigned int *control = (volatile unsigned int *)((char *)page + CONTROL_OFFSET);
        snprintf(line, sizeof(line), "pci bridge control 0x%08x\n", *control);
        log_line(line);
        munmap(page, PAGE_SIZE_BYTES);
    }
    close(fd);
}


static long long nanoseconds_now(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return now.tv_sec * 1000000000LL + now.tv_nsec;
}

// Logs how long the operations on the Wi-Fi data path take, to find out where
// the CPU time per packet goes. The revision register is safe to read.
static void log_access_speed(void) {
    enum {
        WIFI_PAGE = 0x1a004000, WIFI_REVISION_OFFSET = 0x20, PAGE_SIZE_BYTES = 4096,
        REGISTER_READS = 20000, COPY_BYTES = 1 << 20, COPY_ROUNDS = 8,
    };
    static char copy_source[COPY_BYTES];
    static char copy_target[COPY_BYTES];
    char line[128];

    int fd = open("/dev/mem", O_RDONLY | O_SYNC);
    if (fd < 0) {
        return;
    }
    volatile char *wifi = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ, MAP_SHARED, fd, WIFI_PAGE);
    close(fd);
    if (wifi == MAP_FAILED) {
        return;
    }

    long long start = nanoseconds_now();
    for (int read_count = 0; read_count < REGISTER_READS; read_count++) {
        register_at(wifi, WIFI_REVISION_OFFSET);
    }
    long long register_ns = (nanoseconds_now() - start) / REGISTER_READS;
    munmap((void *)wifi, PAGE_SIZE_BYTES);

    start = nanoseconds_now();
    for (int round = 0; round < COPY_ROUNDS; round++) {
        memcpy(copy_target, copy_source, COPY_BYTES);
        // Without this barrier the compiler removes the unused copy.
        __asm__("" : : "r"(copy_target), "r"(copy_source) : "memory");
    }
    long long copy_ns = nanoseconds_now() - start;
    long long copy_mb_per_second = (long long)COPY_ROUNDS * COPY_BYTES * 1000 / copy_ns;

    snprintf(line, sizeof(line), "speed: wifi register read %lld ns, memcpy %lld MB/s\n", register_ns, copy_mb_per_second);
    log_line(line);
}


// Starts hostapd with its output in the kernel log.
static void start_access_point(void) {
    pid_t pid = fork();
    if (pid < 0) {
        log_line("cannot fork hostapd\n");
        return;
    }
    if (pid == 0) {
        if (kmsg_fd >= 0) {
            dup2(kmsg_fd, STDOUT_FILENO);
            dup2(kmsg_fd, STDERR_FILENO);
        }
        execl("/usr/sbin/hostapd", "hostapd", "/etc/hostapd-2ghz.conf", "/etc/hostapd-5ghz.conf", (char *)NULL);
        log_line("cannot start hostapd\n");
        _exit(1);
    }
}

// Gives an access point an address, so that a client can reach this box.
static void configure_address(const char *interface, const char *ip_address) {
    struct ifreq request;
    memset(&request, 0, sizeof(request));
    strncpy(request.ifr_name, interface, IFNAMSIZ - 1);

    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) {
        log_line("cannot open a socket for the address\n");
        return;
    }

    struct sockaddr_in *address = (struct sockaddr_in *)&request.ifr_addr;
    address->sin_family = AF_INET;
    inet_pton(AF_INET, ip_address, &address->sin_addr);
    if (ioctl(fd, SIOCSIFADDR, &request) != 0) {
        log_line("cannot set the address\n");
    }

    inet_pton(AF_INET, AP_NETMASK, &address->sin_addr);
    if (ioctl(fd, SIOCSIFNETMASK, &request) != 0) {
        log_line("cannot set the netmask\n");
    }
    close(fd);
}

static void set_interface_up(int fd, const char *interface) {
    struct ifreq request;
    char line[96];
    memset(&request, 0, sizeof(request));
    strncpy(request.ifr_name, interface, IFNAMSIZ - 1);
    if (ioctl(fd, SIOCGIFFLAGS, &request) != 0) {
        return;
    }
    request.ifr_flags |= IFF_UP;
    if (ioctl(fd, SIOCSIFFLAGS, &request) != 0) {
        snprintf(line, sizeof(line), "cannot bring up %s: %s\n", interface, strerror(errno));
        log_line(line);
    }
}

// Creates the bridge between the wired network and the 2.4 GHz network. hostapd
// adds the Wi-Fi interface itself.
static void create_bridge(void) {
    char line[96];
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) {
        return;
    }
    if (ioctl(fd, SIOCBRADDBR, BRIDGE_INTERFACE) != 0) {
        snprintf(line, sizeof(line), "cannot create %s: %s\n", BRIDGE_INTERFACE, strerror(errno));
        log_line(line);
    }
    close(fd);
}

// Adds the USB Ethernet adapter to the bridge. It appears a few seconds after boot.
static void connect_wired_network(void) {
    struct ifreq request;
    char line[96];

    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) {
        return;
    }
    memset(&request, 0, sizeof(request));
    strncpy(request.ifr_name, WIRED_INTERFACE, IFNAMSIZ - 1);

    int attempts = WIRED_WAIT_S;
    while (attempts > 0 && ioctl(fd, SIOCGIFINDEX, &request) != 0) {
        struct timespec delay = {1, 0};
        nanosleep(&delay, NULL);
        attempts--;
    }
    if (attempts == 0) {
        log_line("no USB Ethernet interface appeared\n");
        close(fd);
        return;
    }

    int wired_index = request.ifr_ifindex;
    memset(&request, 0, sizeof(request));
    strncpy(request.ifr_name, BRIDGE_INTERFACE, IFNAMSIZ - 1);
    request.ifr_ifindex = wired_index;
    if (ioctl(fd, SIOCBRADDIF, &request) != 0) {
        snprintf(line, sizeof(line), "cannot add %s to %s: %s\n", WIRED_INTERFACE, BRIDGE_INTERFACE, strerror(errno));
        log_line(line);
    }

    set_interface_up(fd, WIRED_INTERFACE);
    close(fd);

    configure_address(BRIDGE_INTERFACE, BRIDGE_ADDRESS);

    fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd >= 0) {
        set_interface_up(fd, BRIDGE_INTERFACE);
        close(fd);
    }
    snprintf(line, sizeof(line), "%s has %s and contains %s\n", BRIDGE_INTERFACE, BRIDGE_ADDRESS, WIRED_INTERFACE);
    log_line(line);
}

static int listen_on_port(int port) {
    struct sockaddr_in address;
    memset(&address, 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = htonl(INADDR_ANY);
    address.sin_port = htons(port);

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0 || bind(fd, (struct sockaddr *)&address, sizeof(address)) != 0 || listen(fd, 1) != 0) {
        log_line("cannot listen on a test port\n");
        _exit(1);
    }
    return fd;
}

// Sends data to each client until it disconnects, to test the box's transmit path.
static void run_send_service(void) {
    static char buffer[TRANSFER_BUFFER_SIZE];
    int server = listen_on_port(SEND_PORT);
    while (1) {
        int client = accept(server, NULL, NULL);
        if (client < 0) {
            continue;
        }
        while (write(client, buffer, sizeof(buffer)) > 0) {
        }
        close(client);
    }
}

// Discards what each client sends and logs the amount, to test the receive path.
static void run_receive_service(void) {
    static char buffer[TRANSFER_BUFFER_SIZE];
    char line[64];
    int server = listen_on_port(RECEIVE_PORT);
    while (1) {
        int client = accept(server, NULL, NULL);
        if (client < 0) {
            continue;
        }
        long long total = 0;
        ssize_t length;
        while ((length = read(client, buffer, sizeof(buffer))) > 0) {
            total += length;
        }
        snprintf(line, sizeof(line), "received %lld bytes\n", total);
        log_line(line);
        close(client);
    }
}

static void start_transfer_services(void) {
    signal(SIGPIPE, SIG_IGN);
    if (fork() == 0) {
        run_send_service();
    }
    if (fork() == 0) {
        run_receive_service();
    }
}

static void log_exited_children(void) {
    char line[128];
    int status;
    pid_t pid;
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
        snprintf(line, sizeof(line), "process %d ended with status 0x%x\n", (int)pid, status);
        log_line(line);
    }
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
    if (mount("sysfs", "/sys", "sysfs", 0, NULL) != 0) {
        snprintf(line, sizeof(line), "mount /sys: %s\n", strerror(errno));
        log_line(line);
    }
    load_fpga();
    log_file("/proc/cpuinfo");
    log_file("/proc/iomem");
    log_file("/proc/bus/pci/devices");
    log_file("/proc/mtd");

    log_bridge_state();
    create_bridge();
    start_access_point();
    start_transfer_services();
    // The packet processors may use the same MDIO bus, so the switch comes first.
    log_switch_identity();
    configure_switch();
    connect_wired_network();
    log_access_speed();

    long long start = uptime_seconds();
    while (TEST_DURATION_S == 0 || uptime_seconds() - start < TEST_DURATION_S) {
        struct timespec delay = {TICK_S, 0};
        nanosleep(&delay, NULL);
        log_exited_children();
        snprintf(line, sizeof(line), "tick %lld s\n", uptime_seconds());
        log_line(line);
        log_bridge_state();
        log_cpu_usage();
        if ((uptime_seconds() - start) % STATUS_INTERVAL_S < TICK_S) {
            log_link_speeds();
            log_file("/proc/net/dev");
            log_file("/proc/interrupts");
        }
    }

    log_file("/proc/net/dev");
    log_file("/proc/interrupts");
    log_line("Test finished, restarting\n");
    reboot(RB_AUTOBOOT);

    snprintf(line, sizeof(line), "reboot failed: %s\n", strerror(errno));
    log_line(line);
    return 1;
}
