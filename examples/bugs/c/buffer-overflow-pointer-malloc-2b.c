#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main(int argc, char** argv) {
  int* a;

  if (argc < 2)
    a = malloc(90 * sizeof(long));
  else
    a = malloc(100 * sizeof(long));

  /*@{Safe:BufferOverflow*/ a[89] = 1  /*@:Safe}*/;
  /*@{Safe:BufferOverflow*/ a[91] = 1  /*@:Safe}*/;
  /*@{Safe:BufferOverflow*/ a[101] = 1 /*@:Safe}*/;

  return 0;
}
