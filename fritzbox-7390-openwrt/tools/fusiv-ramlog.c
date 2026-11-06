// Prints the RAM log that the bring-up kernel leaves behind.
// It maps /dev/mem read-only and runs in the AVM firmware after a software reset.
// Usage: fusiv-ramlog dump [offset]
//        fusiv-ramlog scan
// Each call prints at most MAX_OUTPUT bytes, so that long logs can be read in parts.

#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

enum {
    LOG_SIZE = 256 * 1024,
    DATA_OFFSET = 0x10,
    MAX_OUTPUT = 30000,
    RAM_SIZE = 128 * 1024 * 1024,
    MARK_SPACING = 64 * 1024,
};

static const uint32_t LOG_PHYS = 0x00fb0000;
static const uint32_t LOG_MAGIC = 0x46534c47;
static const uint32_t MARK_MAGIC = 0x46534d4b;

static const char *stage_name(uint32_t stage) {
    switch (stage) {
    case 1: return "prom_init";
    case 2: return "mem_setup";
    case 3: return "irq_init";
    case 4: return "time_init";
    case 5: return "timer_running";
    case 6: return "restart";
    default: return "unknown";
    }
}

static int dump(uint32_t offset) {
    // O_SYNC requests an uncached mapping.
    int fd = open("/dev/mem", O_RDONLY | O_SYNC);
    if (fd < 0) {
        perror("open /dev/mem");
        return 1;
    }

    void *mapping = mmap(NULL, LOG_SIZE, PROT_READ, MAP_SHARED, fd, LOG_PHYS);
    if (mapping == MAP_FAILED) {
        perror("mmap");
        close(fd);
        return 1;
    }

    const volatile uint32_t *header = mapping;
    const volatile char *data = (const volatile char *)mapping + DATA_OFFSET;
    uint32_t magic = header[0];
    uint32_t length = header[1];
    uint32_t stage = header[2];

    printf("magic=0x%08x length=%u stage=%u (%s)\n", magic, length, stage, stage_name(stage));
    if (magic != LOG_MAGIC) {
        printf("no log found\n");
    } else if (length <= LOG_SIZE - DATA_OFFSET) {
        for (uint32_t i = offset; i < length && i < offset + MAX_OUTPUT; i++) {
            putchar(data[i]);
        }
    } else {
        printf("invalid length\n");
    }
    fflush(stdout);

    munmap(mapping, LOG_SIZE);
    close(fd);
    return 0;
}

// Lists the memory ranges in which the marks of the bring-up kernel survived.
static int scan(void) {
    int fd = open("/dev/mem", O_RDONLY | O_SYNC);
    if (fd < 0) {
        perror("open /dev/mem");
        return 1;
    }

    const volatile char *ram = mmap(NULL, RAM_SIZE, PROT_READ, MAP_SHARED, fd, 0);
    if (ram == MAP_FAILED) {
        perror("mmap");
        close(fd);
        return 1;
    }

    uint32_t survived = 0;
    int in_range = 0;
    uint32_t range_start = 0;
    for (uint32_t address = 0; address < RAM_SIZE; address += MARK_SPACING) {
        const volatile uint32_t *mark = (const volatile uint32_t *)(ram + address);
        int valid = mark[0] == MARK_MAGIC && mark[1] == address && mark[2] == ~address;
        if (valid) {
            survived++;
        }
        if (valid && !in_range) {
            range_start = address;
            in_range = 1;
        } else if (!valid && in_range) {
            printf("0x%08x-0x%08x\n", range_start, address);
            in_range = 0;
        }
    }
    if (in_range) {
        printf("0x%08x-0x%08x\n", range_start, RAM_SIZE);
    }
    printf("%u of %u marks survived\n", survived, RAM_SIZE / MARK_SPACING);
    fflush(stdout);

    munmap((void *)ram, RAM_SIZE);
    close(fd);
    return 0;
}

int main(int argc, char **argv) {
    if (argc >= 2 && strcmp(argv[1], "dump") == 0) {
        uint32_t offset = argc >= 3 ? strtoul(argv[2], NULL, 0) : 0;
        return dump(offset);
    }
    if (argc == 2 && strcmp(argv[1], "scan") == 0) {
        return scan();
    }
    puts("fusiv-ramlog: run with 'dump [offset]' or 'scan'");
    return 0;
}
