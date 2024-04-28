#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern void printflag();
char *gets(char *);

int main(int argc, char **argv) {
  struct {
    char buffer[64];
    volatile int changeme;
  } locals;

  puts("Welcome to stack0, brought to you by https://exploit.education");

  locals.changeme = 0;
  gets(locals.buffer);

  if (locals.changeme != 0) {
    puts("Well done, the 'changeme' variable has been changed!");
    printflag();
  } else {
    puts("Uh oh, 'changeme' has not yet been changed. Would you like to try "
        "again?");
  }

  exit(0);
}
