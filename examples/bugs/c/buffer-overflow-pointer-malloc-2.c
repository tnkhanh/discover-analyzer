#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main(int argc, char** argv) {
  int* a;

  if (argc < 2)
    a = malloc(90 * sizeof(int));
  else
    a = malloc(100 * sizeof(int));

  /*@{Safe:BufferOverflow*/ a[89] = 1  /*@:Safe}*/;
  /*@{Bug:BufferOverflow*/  a[91] = 1  /*@:Bug}*/;
  /*@{Bug:BufferOverflow*/  a[101] = 1 /*@:Bug}*/;

  return 0;
}
