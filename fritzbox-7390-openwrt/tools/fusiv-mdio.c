// Shows the registers of both Ethernet blocks and which of them reaches PHYs
// over MDIO. Run it under the AVM firmware to compare with the new kernel.
//
// The reads go around the AVM driver, so a single value can collide with its
// own MDIO polling. They only read, and repeating the run shows such a collision.
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

enum {
    PAGE_SIZE_BYTES = 4096,
    DUMP_BYTES = 0x100,
    MDIO_WRITE_DATA = 0x80,
    MDIO_ADDRESS = 0x84,
    MDIO_READ_START = 0x88,
    MDIO_STATUS = 0x8c,
    MDIO_READ_DATA = 0x90,
    MDIO_SETTLE_US = 200,
    MDIO_READ_BUSY = 1,
    MDIO_WRITE_BUSY = 2,
    SWITCH_PAGE_PHY = 0x18,
    MDIO_POLL_LIMIT = 100000,
    MDIO_REGISTER_BITS = 5,
    PHY_COUNT = 32,
    PHY_ID_HIGH = 2,
    PHY_ID_LOW = 3,
    NO_DEVICE = 0xffff,
    GPIO_REGISTERS = 0x19040000,
    GPIO_BYTES = 0x20,
    MAC_REGISTERS = 0x19110800,
    MAC_BYTES = 0x400,
};

static const unsigned int ETHERNET_BLOCKS[] = {0x19110000, 0x19150000};

static unsigned int read_register(volatile char *block, unsigned int offset) {
    return *(volatile unsigned int *)(block + offset);
}

static int mdio_read(volatile char *block, unsigned int phy, unsigned int reg) {
    *(volatile unsigned int *)(block + MDIO_ADDRESS) = (phy << MDIO_REGISTER_BITS) | reg;
    read_register(block, MDIO_READ_START);
    usleep(MDIO_SETTLE_US);
    for (int attempt = 0; attempt < MDIO_POLL_LIMIT; attempt++) {
        if ((read_register(block, MDIO_STATUS) & MDIO_READ_BUSY) == 0) {
            return read_register(block, MDIO_READ_DATA) & 0xffff;
        }
    }
    return -1;
}

static void mdio_write(volatile char *block, unsigned int phy, unsigned int reg, unsigned int value) {
    *(volatile unsigned int *)(block + MDIO_ADDRESS) = (phy << MDIO_REGISTER_BITS) | reg;
    *(volatile unsigned int *)(block + MDIO_WRITE_DATA) = value;
    for (int attempt = 0; attempt < MDIO_POLL_LIMIT; attempt++) {
        if ((read_register(block, MDIO_STATUS) & MDIO_WRITE_BUSY) == 0) {
            return;
        }
    }
}

// Shows the switch setup. Selecting a register page can collide with the MDIO
// polling of the AVM driver, so run it twice and compare.
static void show_switch(int fd) {
    static const unsigned int REGISTERS[] = {
        0x0000, 0x0004, 0x0008, 0x000c, 0x0010, 0x0014, 0x0020, 0x0024, 0x0028, 0x002c, 0x0030, 0x0038, 0x0060, 0x0078,
        0x0100, 0x0104, 0x0108, 0x010c, 0x0200, 0x0204, 0x0208, 0x020c, 0x0300, 0x0304, 0x0308, 0x030c,
        0x0400, 0x0404, 0x0408, 0x040c, 0x0500, 0x0504, 0x0508, 0x050c, 0x0600, 0x0604, 0x0608, 0x060c,
    };
    volatile char *block = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ | PROT_WRITE, MAP_SHARED, fd, ETHERNET_BLOCKS[0]);
    if (block == MAP_FAILED) {
        return;
    }
    for (size_t i = 0; i < sizeof(REGISTERS) / sizeof(REGISTERS[0]); i++) {
        unsigned int address = REGISTERS[i];
        unsigned int word_phy = 0x10 | ((address >> 6) & 0x7);
        unsigned int word_reg = (address >> 1) & 0x1f;
        mdio_write(block, SWITCH_PAGE_PHY, 0, (address >> 9) & 0x3ff);
        int low = mdio_read(block, word_phy, word_reg);
        int high = mdio_read(block, word_phy, word_reg + 1);
        printf("switch 0x%04x 0x%04x%04x\n", address, high & 0xffff, low & 0xffff);
    }
    munmap((void *)block, PAGE_SIZE_BYTES);
}

static void show_range(int fd, unsigned int address, unsigned int bytes) {
    volatile char *page = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ, MAP_SHARED, fd, address & ~(PAGE_SIZE_BYTES - 1u));
    if (page == MAP_FAILED) {
        printf("cannot map 0x%08x\n", address);
        return;
    }
    unsigned int page_offset = address & (PAGE_SIZE_BYTES - 1u);
    for (unsigned int offset = 0; offset < bytes; offset += 4) {
        printf("reg 0x%08x 0x%08x\n", address + offset, read_register(page, page_offset + offset));
    }
    munmap((void *)page, PAGE_SIZE_BYTES);
}

static void show_block(int fd, unsigned int address) {
    volatile char *block = mmap(NULL, PAGE_SIZE_BYTES, PROT_READ | PROT_WRITE, MAP_SHARED, fd, address);
    if (block == MAP_FAILED) {
        printf("cannot map 0x%08x\n", address);
        return;
    }

    printf("block 0x%08x\n", address);
    for (unsigned int offset = 0; offset < DUMP_BYTES; offset += 4) {
        // This offset starts an MDIO read, and the value belongs to the driver.
        if (offset == MDIO_READ_START) {
            continue;
        }
        printf("  +0x%03x 0x%08x\n", offset, read_register(block, offset));
    }

    for (unsigned int phy = 0; phy < PHY_COUNT; phy++) {
        int high = mdio_read(block, phy, PHY_ID_HIGH);
        int low = mdio_read(block, phy, PHY_ID_LOW);
        if (high != NO_DEVICE || low != NO_DEVICE) {
            printf("  phy %2u id %d 0x%04x 0x%04x\n", phy, high != -1, high & 0xffff, low & 0xffff);
        }
    }
    munmap((void *)block, PAGE_SIZE_BYTES);
}

int main(void) {
    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    if (fd < 0) {
        perror("/dev/mem");
        return 1;
    }
    for (size_t i = 0; i < sizeof(ETHERNET_BLOCKS) / sizeof(ETHERNET_BLOCKS[0]); i++) {
        show_block(fd, ETHERNET_BLOCKS[i]);
    }
    show_range(fd, GPIO_REGISTERS, GPIO_BYTES);
    show_range(fd, MAC_REGISTERS, MAC_BYTES);
    show_switch(fd);
    close(fd);
    return 0;
}
