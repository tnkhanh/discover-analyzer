#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  unsigned int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  // BUG: there is an integer overflow bug in the below line
  unsigned int x = /*{Bug:IntegerOverflow*/ a * 4 /*:Bug}*/;

  // BUG: there is an integer overflow bug in the below line
  unsigned long y = /*{Bug:IntegerOverflow*/ a * 10 /*:Bug}*/;

  // SAFE: there is NO integer overflow bug in the below line
  // The conversion `long b = a` implies that `b * 10` is SAFE from integer overflow.
  unsigned long b = a;
  unsigned long z = /*{Safe:IntegerOverflow*/ b * 10 /*:Safe}*/;

  printf("x: %u\n", x);
  printf("y: %lu\n", y);
  return 0;
}
