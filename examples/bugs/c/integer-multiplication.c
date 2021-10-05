#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  // There is 1 integer overflow and 1 integer underflow bug in the below line
  int x = /*{Bug:IntegerOverflow*/ a * 4 /*:Bug}*/;

  // There is 1 integer overflow and 1 integer underflow bug in the below line
  long y = /*{Bug:IntegerOverflow*/ a * 10 /*:Bug}*/;

  long b = a;
  // There is no integer overflow/underflow bug in the below line
  long z = b * 10;

  printf("x: %d\n", x);
  printf("y: %lu\n", y);
  return 0;
}
