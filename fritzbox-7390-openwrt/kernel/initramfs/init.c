// First userspace program for bring-up. It shows that user mode, the timer and
// both directions of the serial console work.

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <stdio.h>
#include <sys/mount.h>
#include <sys/utsname.h>
#include <time.h>
#include <unistd.h>

enum { TICK_MS = 5000 };

static void print_file(const char *path) {
    char buffer[1024];
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        perror(path);
        return;
    }

    printf("--- %s\n", path);
    fflush(stdout);

    ssize_t length;
    while ((length = read(fd, buffer, sizeof(buffer))) > 0) {
        write(STDOUT_FILENO, buffer, length);
    }
    close(fd);
}

int main(void) {
    struct utsname name;
    uname(&name);
    printf("\nHello from userspace on %s %s\n", name.sysname, name.release);

    if (mount("proc", "/proc", "proc", 0, NULL) != 0) {
        perror("mount /proc");
    }
    print_file("/proc/cpuinfo");
    print_file("/proc/interrupts");
    print_file("/proc/iomem");

    printf("\nTyped characters are echoed. A tick shows that the timer runs.\n");
    fflush(stdout);

    struct pollfd input = {.fd = STDIN_FILENO, .events = POLLIN};
    while (1) {
        int ready = poll(&input, 1, TICK_MS);
        if (ready > 0) {
            char buffer[64];
            ssize_t length = read(STDIN_FILENO, buffer, sizeof(buffer));
            if (length > 0) {
                write(STDOUT_FILENO, buffer, length);
            }
        } else if (ready == 0) {
            struct timespec now;
            clock_gettime(CLOCK_MONOTONIC, &now);
            printf("tick %lld s\n", (long long)now.tv_sec);
            fflush(stdout);
        }
    }
}
