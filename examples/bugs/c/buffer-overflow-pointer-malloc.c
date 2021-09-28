#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main(int argc, char** argv) {
  int* a = malloc(90 * sizeof(int));

  /*{Safe:BufferOverflow*/ a[89] /*:Safe}*/ = 2;

  /*{Bug:BufferOverflow*/ a[91] /*:Bug}*/ = 2;

  return 0;
}
