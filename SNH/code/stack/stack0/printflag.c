#include <unistd.h>

void printflag() {
    char* argv[] = {
        "/bin/cat",
        "flag.txt",
        NULL
    };

    execv(argv[0], argv);    
}
