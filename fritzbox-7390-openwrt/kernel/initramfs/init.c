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

enum {
    TICK_S = 15,
    STATUS_INTERVAL_S = 120,
    SEND_PORT = 5001,
    RECEIVE_PORT = 5002,
    TRANSFER_BUFFER_SIZE = 16384,
    WIRED_WAIT_S = 20,
};

static const char AP_NETMASK[] = "255.255.255.0";
static const char WIRED_INTERFACE[] = "eth0";
static const char BRIDGE_INTERFACE[] = "br0";
static const char BRIDGE_ADDRESS[] = "192.168.0.250";

#ifndef TEST_DURATION_S
#define TEST_DURATION_S 180
#endif

static int kmsg_fd = -1;

static void log_line(const char *line) {
    fputs(line, stdout);
    fflush(stdout);
    if (kmsg_fd >= 0) {
        write(kmsg_fd, line, strlen(line));
    }
}

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

// Access to the AR8316 switch through the MDIO registers of the first Ethernet
// block. EVA has already set up this block to load images over the network.
enum {
    SYSTEM_PAGE = 0x19000000, CHIP_ID_OFFSET = 0x3c, CLOCK_GATE_OFFSET = 0xcc, ETHERNET_CLOCKS = 0x18,
    ETHERNET_PAGE = 0x19110000,
    MDIO_WRITE_DATA = 0x80, MDIO_ADDRESS = 0x84, MDIO_READ_START = 0x88, MDIO_STATUS = 0x8c, MDIO_READ_DATA = 0x90,
    MDIO_READ_BUSY = 1, MDIO_WRITE_BUSY = 2, MDIO_POLL_LIMIT = 100000,
    MDIO_REGISTER_BITS = 5, MDIO_FIELD_MASK = 0x1f,
    SWITCH_PAGE_PHY = 0x18, SWITCH_REGISTER_PHY = 0x10,
    SWITCH_ID_REGISTER = 0, SWITCH_PORT_STATUS_BASE = 0x100, SWITCH_PORT_STATUS_STEP = 0x100,
    GPIO_FLAGS_OFFSET = 0x4, GPIO_SET_OFFSET = 0x6, SWITCH_RESET_PIN = 1 << 15, SWITCH_RESET_WAIT_NS = 500000000,
    GPIO_REGISTERS = 0x19040000,
    SWITCH_PORTS = 6, SWITCH_PHYS = 5,
    PHY_STATUS = 1, PHY_ID_HIGH = 2, PHY_ID_LOW = 3, PHY_LINK_UP = 1 << 2,
    MDIO_FAILED = -1, MDIO_NO_DEVICE = 0xffff,
};

static volatile char *ethernet_registers;

static unsigned int register_at(volatile char *base, unsigned int offset) {
    return *(volatile unsigned int *)(base + offset);
}

static void set_register_at(volatile char *base, unsigned int offset, unsigned int value) {
    *(volatile unsigned int *)(base + offset) = value;
}

static int mdio_wait(unsigned int busy_bits) {
    for (int attempt = 0; attempt < MDIO_POLL_LIMIT; attempt++) {
        if ((register_at(ethernet_registers, MDIO_STATUS) & busy_bits) == 0) {
            return 0;
        }
    }
    return MDIO_FAILED;
}

static unsigned int mdio_address(unsigned int phy, unsigned int reg) {
    return ((phy & MDIO_FIELD_MASK) << MDIO_REGISTER_BITS) | (reg & MDIO_FIELD_MASK);
}

static int mdio_read(unsigned int phy, unsigned int reg) {
    if (mdio_wait(MDIO_READ_BUSY | MDIO_WRITE_BUSY) != 0) {
        return MDIO_FAILED;
    }
    set_register_at(ethernet_registers, MDIO_ADDRESS, mdio_address(phy, reg));
    register_at(ethernet_registers, MDIO_READ_START);
    if (mdio_wait(MDIO_READ_BUSY) != 0) {
        return MDIO_FAILED;
    }
    return register_at(ethernet_registers, MDIO_READ_DATA) & 0xffff;
}

static int mdio_write(unsigned int phy, unsigned int reg, unsigned int value) {
    if (mdio_wait(MDIO_READ_BUSY | MDIO_WRITE_BUSY) != 0) {
        return MDIO_FAILED;
    }
    set_register_at(ethernet_registers, MDIO_ADDRESS, mdio_address(phy, reg));
    set_register_at(ethernet_registers, MDIO_WRITE_DATA, value);
    return mdio_wait(MDIO_WRITE_BUSY);
}

// A switch register is reached by selecting its page, then two 16 bit reads.
static long long switch_read(unsigned int address) {
    unsigned int word_phy = SWITCH_REGISTER_PHY | ((address >> 6) & 0x7);
    unsigned int word_reg = (address >> 1) & MDIO_FIELD_MASK;

    if (mdio_write(SWITCH_PAGE_PHY, 0, (address >> 9) & 0x3ff) != 0) {
        return MDIO_FAILED;
    }
    int low = mdio_read(word_phy, word_reg);
    int high = mdio_read(word_phy, word_reg + 1);
    if (low == MDIO_FAILED || high == MDIO_FAILED) {
        return MDIO_FAILED;
    }
    return ((long long)high << 16) | low;
}

// The switch is held in reset when /init starts, and the pin is active low.
static void release_switch_reset(int fd) {
    enum { PAGE_SIZE_BYTES = 4096 };
    char line[64];

    volatile char *gpio = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ | PROT_WRITE, MAP_SHARED, fd, GPIO_REGISTERS);
    if (gpio == MAP_FAILED) {
        log_line("switch: cannot map the GPIO registers\n");
        return;
    }

    snprintf(line, sizeof(line), "switch: gpio flags before release 0x%04x\n", *(volatile unsigned short *)(gpio + GPIO_FLAGS_OFFSET));
    log_line(line);
    *(volatile unsigned short *)(gpio + GPIO_SET_OFFSET) = SWITCH_RESET_PIN;

    struct timespec delay = {0, SWITCH_RESET_WAIT_NS};
    nanosleep(&delay, NULL);

    snprintf(line, sizeof(line), "switch: gpio flags after release 0x%04x\n", *(volatile unsigned short *)(gpio + GPIO_FLAGS_OFFSET));
    log_line(line);
    munmap((void *)gpio, PAGE_SIZE_BYTES);
}

// Logs the switch identity. The Ethernet block stays unmapped if its clocks are
// off, because reading a block without a clock can hang the bus.
static void log_switch_identity(void) {
    enum { PAGE_SIZE_BYTES = 4096 };
    char line[128];

    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (fd < 0) {
        log_line("switch: cannot open /dev/mem\n");
        return;
    }

    volatile char *system_registers = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ, MAP_SHARED, fd, SYSTEM_PAGE);
    if (system_registers == MAP_FAILED) {
        log_line("switch: cannot map the system registers\n");
        close(fd);
        return;
    }
    unsigned int clock_gate = register_at(system_registers, CLOCK_GATE_OFFSET);
    snprintf(line, sizeof(line), "switch: chip id 0x%08x, clock gate 0x%08x\n",
             register_at(system_registers, CHIP_ID_OFFSET), clock_gate);
    log_line(line);
    munmap((void *)system_registers, PAGE_SIZE_BYTES);
    if ((clock_gate & ETHERNET_CLOCKS) != ETHERNET_CLOCKS) {
        log_line("switch: Ethernet clocks are off, not touching the block\n");
        close(fd);
        return;
    }

    release_switch_reset(fd);

    volatile char *registers = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ | PROT_WRITE, MAP_SHARED, fd, ETHERNET_PAGE);
    close(fd);
    if (registers == MAP_FAILED) {
        log_line("switch: cannot map the Ethernet registers\n");
        return;
    }
    ethernet_registers = registers;
    snprintf(line, sizeof(line), "switch: mdio status 0x%08x\n", register_at(registers, MDIO_STATUS));
    log_line(line);

    snprintf(line, sizeof(line), "switch: id register 0x%08llx\n", switch_read(SWITCH_ID_REGISTER));
    log_line(line);
    for (unsigned int phy = 0; phy < SWITCH_PHYS; phy++) {
        snprintf(line, sizeof(line), "switch: phy %u id 0x%04x 0x%04x status 0x%04x\n", phy,
                 mdio_read(phy, PHY_ID_HIGH), mdio_read(phy, PHY_ID_LOW), mdio_read(phy, PHY_STATUS));
        log_line(line);
    }
    for (unsigned int port = 0; port < SWITCH_PORTS; port++) {
        unsigned int address = SWITCH_PORT_STATUS_BASE + port * SWITCH_PORT_STATUS_STEP;
        snprintf(line, sizeof(line), "switch: port %u status 0x%08llx\n", port, switch_read(address));
        log_line(line);
    }
}

// Logs which of the switch's PHYs have a link, one digit each.
static void log_switch_links(void) {
    char line[64];
    char links[SWITCH_PHYS + 1];

    if (!ethernet_registers) {
        return;
    }
    for (unsigned int phy = 0; phy < SWITCH_PHYS; phy++) {
        int status = mdio_read(phy, PHY_STATUS);
        if (status == MDIO_FAILED || status == MDIO_NO_DEVICE) {
            links[phy] = '?';
        } else {
            links[phy] = (status & PHY_LINK_UP) ? '1' : '0';
        }
    }
    links[SWITCH_PHYS] = '\0';
    snprintf(line, sizeof(line), "switch: links %s\n", links);
    log_line(line);
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

// Programs the board's Xilinx FPGA in slave serial mode, if the image contains
// a bitstream. Under the AVM firmware the USB port gets power around this step.
static void load_fpga(void) {
    enum {
        GPIO_PAGE = 0x19040000, GPIO_DIR = 0x0, GPIO_FLAG = 0x4, GPIO_CLEAR = 0x4, GPIO_SET = 0x6,
        PIN_CLK = 1 << 11, PIN_DATA = 1 << 12, PIN_PROG = 1 << 14, PIN_DONE = 1 << 8,
        PROG_LOW_US = 1000, PROG_HIGH_US = 10000, EXTRA_CLOCKS = 64, PAGE_SIZE_BYTES = 4096,
    };
    static const unsigned char sync_word[] = {0xff, 0xff, 0xff, 0xff, 0xaa, 0x99, 0x55, 0x66};
    static unsigned char bitstream[256 * 1024];
    char line[96];

    FILE *file = fopen("/lib/firmware/fpga.bit", "rb");
    if (!file) {
        log_line("no FPGA bitstream in this image\n");
        return;
    }
    size_t size = fread(bitstream, 1, sizeof(bitstream), file);
    fclose(file);

    size_t start = 0;
    while (start + sizeof(sync_word) < size && memcmp(bitstream + start, sync_word, sizeof(sync_word)) != 0) {
        start++;
    }
    if (start + sizeof(sync_word) >= size) {
        log_line("FPGA bitstream has no sync word\n");
        return;
    }

    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (fd < 0) {
        return;
    }
    volatile char *gpio = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ | PROT_WRITE, MAP_SHARED, fd, GPIO_PAGE);
    if (gpio == MAP_FAILED) {
        close(fd);
        return;
    }
    volatile unsigned short *direction = (volatile unsigned short *)(gpio + GPIO_DIR);
    volatile unsigned short *clear_pins = (volatile unsigned short *)(gpio + GPIO_CLEAR);
    volatile unsigned short *set_pins = (volatile unsigned short *)(gpio + GPIO_SET);
    volatile unsigned int *flags = (volatile unsigned int *)(gpio + GPIO_FLAG);

    *direction = *direction | PIN_CLK | PIN_DATA | PIN_PROG;
    *clear_pins = PIN_CLK;
    *clear_pins = PIN_PROG;
    usleep(PROG_LOW_US);
    *set_pins = PIN_PROG;
    usleep(PROG_HIGH_US);

    unsigned int before = *flags;
    for (size_t i = start; i < size; i++) {
        for (int bit = 7; bit >= 0; bit--) {
            if (bitstream[i] & (1 << bit)) {
                *set_pins = PIN_DATA;
            } else {
                *clear_pins = PIN_DATA;
            }
            *set_pins = PIN_CLK;
            *clear_pins = PIN_CLK;
        }
    }
    for (int i = 0; i < EXTRA_CLOCKS; i++) {
        *set_pins = PIN_CLK;
        *clear_pins = PIN_CLK;
    }
    unsigned int after = *flags;

    snprintf(line, sizeof(line), "FPGA: sent %u bytes, flags 0x%08x -> 0x%08x, done pin %d\n",
             (unsigned int)(size - start), before, after, (after & PIN_DONE) != 0);
    log_line(line);

    munmap((void *)gpio, PAGE_SIZE_BYTES);
    close(fd);
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
    connect_wired_network();
    log_switch_identity();
    log_access_speed();

    long long start = uptime_seconds();
    while (uptime_seconds() - start < TEST_DURATION_S) {
        struct timespec delay = {TICK_S, 0};
        nanosleep(&delay, NULL);
        log_exited_children();
        snprintf(line, sizeof(line), "tick %lld s\n", uptime_seconds());
        log_line(line);
        log_bridge_state();
        log_cpu_usage();
        log_switch_links();
        if ((uptime_seconds() - start) % STATUS_INTERVAL_S < TICK_S) {
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
