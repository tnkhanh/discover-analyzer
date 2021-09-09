#include <stdio.h>
#include <string.h>

char password[] = "super_secret";

void foo(void) {
  printf("You hacked me! Here is the secret password: %s\n", password);
  fflush(stdout);
}

int main(int argc, char *argv[]) {
  char buf[4];

  printf("Here is the address of foo: %p\nWhat is your hacking text? ", foo);
  fflush(stdout);

  gets(buf);             // BUG: buffer overflow here: INFER fails to detect

  printf("You entered: %s\n", buf);
  fflush(stdout);

  return 0;
}
