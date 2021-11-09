#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main(int argc, char** argv) {
  int* a;

  if (argc <= 95)
    a = malloc(95 * sizeof(int));
  else
    a = malloc(argc * sizeof(int));

  /*@{Safe:BufferOverflow*/ a[89] = 1  /*@:Safe}*/;
  /*@{Safe:BufferOverflow*/ a[91] = 1  /*@:Safe}*/;
  /*@{Safe:BufferOverflow*/ a[101] = 1 /*@:Safe}*/;

  return 0;
}
