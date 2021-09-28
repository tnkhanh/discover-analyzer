#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a[90];

  /*{Safe:BufferOverflow*/ a[89] /*:Safe}*/ = 2;

  /*{Bug:BufferOverflow*/ a[91] /*:Bug}*/ = 2;

  return 0;
}
