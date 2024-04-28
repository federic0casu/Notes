#include <stdio.h>
#include <errno.h>      // for errno
#include <stdint.h>     // for uint64_t
#include <stdlib.h>     // for exit()
#include <string.h>     // for strerror()
#include <fcntl.h>      // for O_RDONLY

#define BUFFER_SIZE 1024
#define STDIN       0
#define STDOUT      1
#define SYS_READ    0
#define SYS_WRITE   1
#define SYS_OPEN    2
#define SYS_CLOSE   3
#define SYS_EXIT    60

#define ERROR(res)                                 \
if((res) == -1)                                    \
    fprintf(stderr, "cat: %s\n", strerror(errno)); \

#define FILE_ERROR(fd, path)                       \
if ((fd) == -1) {                                  \
    fprintf(stderr, "cat: %s: %s\n", (path),       \
            strerror(errno));                      \
    exit(-1);                                      \
}                                                  \
 

int _syscall(uint64_t, uint64_t, uint64_t, uint64_t);
int _stdin__stdout(); 


int main(int argc, char* argv[]) {
    // No arguments: no file to open. 
    // cat will copy stdin to stdout.
    if (argc == 1) {
        int res = _stdin__stdout();
        ERROR(res); 
        _syscall(res, 0, 0, SYS_EXIT);
    }

    // We start opening files from argv[1] because
    // argv[0] == 'cat'. 
    for (int i = 1; i < argc; i++) {
        int fd = _syscall((uint64_t) argv[i], O_RDONLY, 0, SYS_OPEN);
        
        FILE_ERROR(fd, argv[i]);

        char tmp;
        while (_syscall((uint64_t) fd, (uint64_t) &tmp, sizeof(char), SYS_READ) != 0)
            _syscall(STDOUT, (uint64_t) &tmp, sizeof(char), SYS_WRITE);

        
        FILE_ERROR(_syscall(fd, 0, 0, SYS_CLOSE), argv[i]);
    }
    
    _syscall(0, 0, 0, SYS_EXIT);
}


// Registers used to pass the system call arguments:
// rdi = arg1   rax = syscall number 
// rsi = arg2
// rdx = arg3
int _syscall(uint64_t arg1, uint64_t arg2, uint64_t arg3,
             uint64_t sys_number) {
    int result;
    
    asm volatile ( 
        "movq %1, %%rdi\n"   // Move arg1 to %rdi
        "movq %2, %%rsi\n"   // Move arg2 to %rsi
        "movq %3, %%rdx\n"   // Move arg3 to %rdx
        "movq %4, %%rax\n"   // Move sys_number to %rax
        "syscall\n"
        "movl %%eax, %0\n"   // Move return value to 'result'
        : "=r" (result)      // Output: result
        : "r" (arg1), "r" (arg2), "r" (arg3), "r" (sys_number) // Inputs
        : "%rax", "%rdi", "%rsi", "%rdx", "cc", "memory" // Clobbered registers
    );

    return result;
}


int _stdin__stdout() {
    char tmp, buffer[BUFFER_SIZE];
    int  count = 0, n_read = 0;

    // Read from stdin one byte at time
    while ((n_read = _syscall(STDIN, (uint64_t) &tmp, 1, SYS_READ)) == 1) {
        buffer[count++] = tmp;

        // Check overflow
        if (!(count < BUFFER_SIZE - 1))
            buffer[count++] = tmp = '\n';

        if (tmp == '\n') {
            // Copy temporary buffer to stdout
            if (_syscall(STDOUT, (uint64_t) buffer, count, SYS_WRITE) == -1) 
                return -1;
            // Flush temporary buffer
            count = 0;
        }
    }

    // n_read == -1: error
    // n_read == 0:  everything OK, user pressed Ctrl^D to exit
    return n_read;
}
