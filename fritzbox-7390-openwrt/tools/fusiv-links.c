// Shows the link of each PHY of the AR8316 switch and prints a line whenever one
// changes, so that a cable plugged into a socket shows which PHY it reaches.
// It only reads PHY registers over the MDIO bus of the first Ethernet block.
// Run it under this kernel, whose Ethernet driver does not use MDIO itself.
// Usage: fusiv-links [seconds], default 600. Stop it earlier with Ctrl-C.
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

enum {
    PAGE_SIZE_BYTES = 4096,
    ETHERNET_BLOCK = 0x19110000,
    MDIO_ADDRESS = 0x84,
    MDIO_READ_START = 0x88,
    MDIO_STATUS = 0x8c,
    MDIO_READ_DATA = 0x90,
    MDIO_READ_BUSY = 1,
    MDIO_WRITE_BUSY = 2,
    MDIO_SETTLE_US = 500,
    MDIO_POLL_LIMIT = 100000,
    MDIO_REGISTER_BITS = 5,
    MDIO_FAILED = -1,
    SWITCH_PHYS = 5,
    PHY_SPECIFIC_STATUS = 0x11,
    STATUS_LINK = 1 << 10,
    STATUS_RESOLVED = 1 << 11,
    STATUS_FULL_DUPLEX = 1 << 13,
    STATUS_SPEED_SHIFT = 14,
    DEFAULT_SECONDS = 600,
};

static unsigned int read_register(volatile char *block, unsigned int offset) {
    return *(volatile unsigned int *)(block + offset);
}

static int mdio_wait(volatile char *block, unsigned int busy_bits) {
    for (int attempt = 0; attempt < MDIO_POLL_LIMIT; attempt++) {
        if ((read_register(block, MDIO_STATUS) & busy_bits) == 0) {
            return 0;
        }
    }
    return MDIO_FAILED;
}

// The busy bit only rises some time after the read starts, so wait before polling.
static int mdio_read(volatile char *block, unsigned int phy, unsigned int reg) {
    if (mdio_wait(block, MDIO_READ_BUSY | MDIO_WRITE_BUSY) != 0) {
        return MDIO_FAILED;
    }
    *(volatile unsigned int *)(block + MDIO_ADDRESS) = (phy << MDIO_REGISTER_BITS) | reg;
    read_register(block, MDIO_READ_START);
    usleep(MDIO_SETTLE_US);
    if (mdio_wait(block, MDIO_READ_BUSY) != 0) {
        return MDIO_FAILED;
    }
    return read_register(block, MDIO_READ_DATA) & 0xffff;
}

// Describes a PHY status like "1000 Mbit/s full duplex" or "down".
static void describe(int status, char *text, size_t size) {
    static const char *const SPEEDS[] = {"10", "100", "1000", "unknown"};

    if (status == MDIO_FAILED) {
        snprintf(text, size, "cannot be read");
    } else if (!(status & STATUS_LINK) || !(status & STATUS_RESOLVED)) {
        snprintf(text, size, "down");
    } else {
        snprintf(text, size, "%s Mbit/s %s duplex", SPEEDS[(status >> STATUS_SPEED_SHIFT) & 0x3],
                 (status & STATUS_FULL_DUPLEX) ? "full" : "half");
    }
}

int main(int argc, char **argv) {
    int seconds = argc > 1 ? atoi(argv[1]) : DEFAULT_SECONDS;
    char previous[SWITCH_PHYS][40] = {{0}};
    char current[40];

    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (fd < 0) {
        perror("/dev/mem");
        return 1;
    }
    volatile char *block = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ | PROT_WRITE, MAP_SHARED, fd, ETHERNET_BLOCK);
    if (block == MAP_FAILED) {
        perror("mmap");
        return 1;
    }

    for (int second = 0; second < seconds; second++) {
        for (unsigned int phy = 0; phy < SWITCH_PHYS; phy++) {
            describe(mdio_read(block, phy, PHY_SPECIFIC_STATUS), current, sizeof(current));
            if (strcmp(previous[phy], current) != 0) {
                printf("%4d s  phy %u %s\n", second, phy, current);
                fflush(stdout);
                strcpy(previous[phy], current);
            }
        }
        sleep(1);
    }
    return 0;
}
