// Prints a fixed list of Ikanos Fusiv Vx180 clock and GPIO registers.
// It maps /dev/mem read-only, so it cannot change any register.
// Addresses and widths come from arch/mips/include/asm/mach-fusiv/vx180.h
// in AVM's GPL kernel for the FRITZ!Box 7390.

#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

enum { PAGE = 4096 };

struct reg {
    const char *name;
    uint32_t addr;
};

static const struct reg regs[] = {
    {"GENERAL_STATUS", 0x1900001c},
    {"PCI_CTRL_STAT", 0x19000040},
    {"SYSTEM_CONTROL", 0x19000088},
    {"SYSTEM_PLL_CONTROL", 0x190000ac},
    {"MODE_CONTROL", 0x190000b0},
    {"PROC_PLL_CONTROL", 0x190000b4},
    {"DSP_PLL_CONTROL", 0x190000bc},
    {"USB_PCI_PLL_CONTROL", 0x190000c8},
    {"CLK_GATE_CONTROL", 0x190000cc},
    {"MODE_STATUS", 0x190000d0},
    {"PAD_PIO_MISC_CTRL", 0x190000d4},
    {"GPIO0_DIR", 0x19040000},
    {"GPIO0_FLAG", 0x19040004},
    {"GPIO0_IRQ_ENABLE", 0x19040008},
    {"GPIO0_POLAR", 0x19040010},
    {"GPIO0_SENSITIVITY", 0x19040014},
    {"GPIO0_BOTH_EDGES", 0x19040018},
    {"GPIO1_DIR", 0x190b0000},
    {"GPIO1_FLAG", 0x190b0004},
    {"GPIO1_IRQ_ENABLE", 0x190b0008},
    {"GPIO1_POLAR", 0x190b0010},
    {"GPIO1_SENSITIVITY", 0x190b0014},
    {"GPIO1_BOTH_EDGES", 0x190b0018},
};

static int dump(void) {
    // O_SYNC requests an uncached mapping.
    int fd = open("/dev/mem", O_RDONLY | O_SYNC);
    if (fd < 0) {
        perror("open /dev/mem");
        return 1;
    }

    size_t count = sizeof(regs) / sizeof(regs[0]);
    for (size_t i = 0; i < count; i++) {
        uint32_t page_addr = regs[i].addr & ~(uint32_t)(PAGE - 1);
        uint32_t offset = regs[i].addr - page_addr;

        void *page = mmap(NULL, PAGE, PROT_READ, MAP_SHARED, fd, page_addr);
        if (page == MAP_FAILED) {
            perror("mmap");
            close(fd);
            return 1;
        }

        volatile const uint32_t *reg = (volatile const uint32_t *)((char *)page + offset);
        printf("%-20s 0x%08x = 0x%08x\n", regs[i].name, regs[i].addr, *reg);
        fflush(stdout);
        munmap(page, PAGE);
    }

    close(fd);
    return 0;
}

int main(int argc, char **argv) {
    if (argc == 2 && strcmp(argv[1], "dump") == 0) {
        return dump();
    }
    puts("fusiv-regdump: run with 'dump' to print the registers");
    return 0;
}
