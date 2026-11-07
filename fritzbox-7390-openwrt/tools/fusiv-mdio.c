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
    MDIO_ADDRESS = 0x84,
    MDIO_READ_START = 0x88,
    MDIO_STATUS = 0x8c,
    MDIO_READ_DATA = 0x90,
    MDIO_READ_BUSY = 1,
    MDIO_POLL_LIMIT = 100000,
    MDIO_REGISTER_BITS = 5,
    PHY_COUNT = 32,
    PHY_ID_HIGH = 2,
    PHY_ID_LOW = 3,
    NO_DEVICE = 0xffff,
    GPIO_REGISTERS = 0x19040000,
    GPIO_BYTES = 0x20,
    MAC_REGISTERS = 0x19110800,
    MAC_BYTES = 0x100,
};

static const unsigned int ETHERNET_BLOCKS[] = {0x19110000, 0x19150000};

static unsigned int read_register(volatile char *block, unsigned int offset) {
    return *(volatile unsigned int *)(block + offset);
}

static int mdio_read(volatile char *block, unsigned int phy, unsigned int reg) {
    *(volatile unsigned int *)(block + MDIO_ADDRESS) = (phy << MDIO_REGISTER_BITS) | reg;
    read_register(block, MDIO_READ_START);
    for (int attempt = 0; attempt < MDIO_POLL_LIMIT; attempt++) {
        if ((read_register(block, MDIO_STATUS) & MDIO_READ_BUSY) == 0) {
            return read_register(block, MDIO_READ_DATA) & 0xffff;
        }
    }
    return -1;
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
    close(fd);
    return 0;
}
