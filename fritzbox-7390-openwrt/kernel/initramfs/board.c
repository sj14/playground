// Hardware setup and logging shared by the test init (init.c) and the OpenWrt
// init (openwrt-init.c): the FPGA, the AR8316 switch and the kernel log.

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>

#include "board.h"

int kmsg_fd = -1;

void log_line(const char *line) {
    fputs(line, stdout);
    fflush(stdout);
    if (kmsg_fd >= 0) {
        write(kmsg_fd, line, strlen(line));
    }
}

// Reports in the AVM reboot status that /init really ran, because this text
// survives the reset. The firmware then shows "watchdog".
void report_init_ran(void) {
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

// Access to the AR8316 switch through the MDIO registers of the first Ethernet
// block. EVA has already set up this block to load images over the network.
enum {
    SYSTEM_PAGE = 0x19000000, CHIP_ID_OFFSET = 0x3c, CLOCK_GATE_OFFSET = 0xcc, ETHERNET_CLOCKS = 0x18,
    ETHERNET_PAGE = 0x19110000,
    MDIO_WRITE_DATA = 0x80, MDIO_ADDRESS = 0x84, MDIO_READ_START = 0x88, MDIO_STATUS = 0x8c, MDIO_READ_DATA = 0x90,
    MDIO_READ_BUSY = 1, MDIO_WRITE_BUSY = 2, MDIO_POLL_LIMIT = 100000, MDIO_SETTLE_NS = 500000,
    MDIO_REGISTER_BITS = 5, MDIO_FIELD_MASK = 0x1f,
    SWITCH_PAGE_PHY = 0x18, SWITCH_REGISTER_PHY = 0x10,
    SWITCH_ID_REGISTER = 0, SWITCH_PORT_STATUS_BASE = 0x100, SWITCH_PORT_STATUS_STEP = 0x100,
    GPIO_FLAGS_OFFSET = 0x4, GPIO_SET_OFFSET = 0x6, SWITCH_RESET_PIN = 1 << 15, SWITCH_RESET_WAIT_NS = 500000000,
    GPIO_REGISTERS = 0x19040000,
    SWITCH_PORTS = 6, SWITCH_PHYS = 5,
    PHY_STATUS = 1, PHY_ID_HIGH = 2, PHY_ID_LOW = 3,
    MDIO_FAILED = -1,
};

static volatile char *ethernet_registers;

unsigned int register_at(volatile char *base, unsigned int offset) {
    return *(volatile unsigned int *)(base + offset);
}

static void set_register_at(volatile char *base, unsigned int offset, unsigned int value) {
    *(volatile unsigned int *)(base + offset) = value;
}

// The busy bits only rise some time after a command starts, so a command that
// polls them right away can overlap with the next one and damage it.
static void mdio_settle(void) {
    struct timespec delay = {0, MDIO_SETTLE_NS};
    nanosleep(&delay, NULL);
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
    mdio_settle();
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
    mdio_settle();
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

static int switch_write(unsigned int address, unsigned int value) {
    unsigned int word_phy = SWITCH_REGISTER_PHY | ((address >> 6) & 0x7);
    unsigned int word_reg = (address >> 1) & MDIO_FIELD_MASK;

    if (mdio_write(SWITCH_PAGE_PHY, 0, (address >> 9) & 0x3ff) != 0) {
        return MDIO_FAILED;
    }
    // The switch takes over the value when the low half arrives, so it comes last.
    if (mdio_write(word_phy, word_reg + 1, value >> 16) != 0) {
        return MDIO_FAILED;
    }
    return mdio_write(word_phy, word_reg, value & 0xffff);
}

// Writes a switch register and logs the value that it reads back.
static void switch_set(unsigned int address, unsigned int value) {
    char line[80];

    switch_write(address, value);
    snprintf(line, sizeof(line), "switch: 0x%04x set to 0x%08x, reads 0x%08llx\n", address, value, switch_read(address));
    log_line(line);
}

// Sets up the switch's connection to the Ethernet block like AVM's open driver.
void configure_switch(void) {
    enum {
        SWITCH_STRAP = 0x08, SWITCH_FORWARD_PORTS = 0x2c, SWITCH_CPU_PORT = 0x78,
        SWITCH_PORT0_STATUS = 0x100, SWITCH_PORT0_VLAN = 0x108,
        STRAP_VALUE = 0x000e1b20 | (1 << 0) | (1 << 4) | (1 << 15) | (1 << 24),
        FORWARD_PORTS_VALUE = 0x043f3f3f,
        CPU_PORT_VALUE = 0x1f0,
        // 1000 Mbit/s, full duplex, flow control, both directions on.
        PORT0_STATUS_VALUE = 0x7e,
            PORT0_VLAN_BITS = (1 << 24) | (1 << 2),
    };

    if (!ethernet_registers) {
        return;
    }
    long long vlan = switch_read(SWITCH_PORT0_VLAN);
    if (vlan != MDIO_FAILED) {
        switch_set(SWITCH_PORT0_VLAN, (unsigned int)vlan | PORT0_VLAN_BITS);
    }
    switch_set(SWITCH_PORT0_STATUS, PORT0_STATUS_VALUE);
    switch_set(SWITCH_CPU_PORT, CPU_PORT_VALUE);
    switch_set(SWITCH_FORWARD_PORTS, FORWARD_PORTS_VALUE);
    switch_set(SWITCH_STRAP, STRAP_VALUE);
}

// Logs the link of each switch PHY as the PHY negotiated it.
void log_link_speeds(void) {
    enum {
        PHY_SPECIFIC_STATUS = 0x11,
        STATUS_LINK = 1 << 10,
        STATUS_RESOLVED = 1 << 11,
        STATUS_FULL_DUPLEX = 1 << 13,
        STATUS_SPEED_SHIFT = 14,
    };
    static const char *const SPEEDS[] = {"10", "100", "1000", "unknown"};
    char line[96];

    if (ethernet_registers == NULL) {
        return;
    }
    for (unsigned int phy = 0; phy < SWITCH_PHYS; phy++) {
        int status = mdio_read(phy, PHY_SPECIFIC_STATUS);
        if (status == MDIO_FAILED) {
            snprintf(line, sizeof(line), "link: phy %u cannot be read\n", phy);
        } else if (!(status & STATUS_LINK) || !(status & STATUS_RESOLVED)) {
            snprintf(line, sizeof(line), "link: phy %u down (0x%04x)\n", phy, status);
        } else {
            snprintf(line, sizeof(line), "link: phy %u %s Mbit/s %s duplex (0x%04x)\n", phy,
                     SPEEDS[(status >> STATUS_SPEED_SHIFT) & 0x3],
                     (status & STATUS_FULL_DUPLEX) ? "full" : "half", status);
        }
        log_line(line);
    }
}

// Logs the switch identity. The Ethernet block stays unmapped if its clocks are
// off, because reading a block without a clock can hang the bus.
void log_switch_identity(void) {
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

// Programs the board's Xilinx FPGA in slave serial mode, if the image contains
// a bitstream. Under the AVM firmware the USB port gets power around this step.
void load_fpga(void) {
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
