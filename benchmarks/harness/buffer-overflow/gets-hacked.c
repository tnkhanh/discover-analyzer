#include <stdio.h>
#include <string.h>


void hack(void) {
  printf("You are hacked!");
  // do something malicious here
}

int main(int argc, char *argv[]) {
  char buf[4];

  printf("Input a string? ");
  gets(buf);             // BUG: buffer overflow here: INFER fails to detect

  printf("You entered: %s\n", buf);
  return 0;
}
