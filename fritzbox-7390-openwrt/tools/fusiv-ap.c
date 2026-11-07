// Shows how the AVM firmware has set up the Ikanos packet processors (APs).
// Run it under the AVM firmware. It only reads registers that the AVM driver
// also reads. It must not read 0x19210810, because that takes a buffer off the
// pool of the buffer manager.
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>

enum {
    PAGE_SIZE_BYTES = 4096,
    CONTROL = 0x300,
    INTERRUPT_STATUS = 0x308,
    INTERRUPT_MASK = 0x30c,
    CONFIG_HIGH = 0x310,
    CONFIG_LOW = 0x314,
    CONFIG_EVENT = 0x318,
    CONFIG_BYTES = 352,
    QUEUE_STATUS = 0x504,
    KSEG_MASK = 0x1fffffff,
};

struct processor {
    const char *name;
    unsigned int address;
};

static const struct processor PROCESSORS[] = {
    {"mac1", 0x19260000},
    {"mac2", 0x19270000},
    {"bmu", 0x19210000},
};

static int mem_fd;

static volatile char *map_page(unsigned int address) {
    return mmap(NULL, PAGE_SIZE_BYTES, PROT_READ, MAP_SHARED, mem_fd, address & ~(PAGE_SIZE_BYTES - 1u));
}

static unsigned int read16(volatile char *base, unsigned int offset) {
    return *(volatile unsigned short *)(base + offset);
}

static void show_config(unsigned int address) {
    volatile char *page = map_page(address);
    if (page == MAP_FAILED) {
        printf("  cannot map config 0x%08x\n", address);
        return;
    }
    unsigned int start = address & (PAGE_SIZE_BYTES - 1u);
    for (unsigned int offset = 0; offset < CONFIG_BYTES && start + offset < PAGE_SIZE_BYTES; offset += 4) {
        unsigned int value = *(volatile unsigned int *)(page + start + offset);
        if (value != 0) {
            printf("  config +%3u 0x%08x\n", offset, value);
        }
    }
    munmap((void *)page, PAGE_SIZE_BYTES);
}

static void show_processor(const struct processor *processor) {
    volatile char *base = map_page(processor->address);
    if (base == MAP_FAILED) {
        printf("cannot map %s\n", processor->name);
        return;
    }

    unsigned int config = (read16(base, CONFIG_HIGH) << 16) | read16(base, CONFIG_LOW);
    printf("%s control 0x%04x status 0x%04x mask 0x%04x event 0x%04x queue 0x%04x config 0x%08x\n",
           processor->name, read16(base, CONTROL), read16(base, INTERRUPT_STATUS), read16(base, INTERRUPT_MASK),
           read16(base, CONFIG_EVENT), read16(base, QUEUE_STATUS), config);
    munmap((void *)base, PAGE_SIZE_BYTES);

    if (config != 0 && (config & KSEG_MASK) < 0x08000000) {
        show_config(config & KSEG_MASK);
    }
}

int main(void) {
    mem_fd = open("/dev/mem", O_RDONLY | O_SYNC);
    if (mem_fd < 0) {
        perror("/dev/mem");
        return 1;
    }
    for (size_t i = 0; i < sizeof(PROCESSORS) / sizeof(PROCESSORS[0]); i++) {
        show_processor(&PROCESSORS[i]);
    }
    close(mem_fd);
    return 0;
}
