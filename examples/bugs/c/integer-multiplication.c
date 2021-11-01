#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  // There are potential integer overflow/underflow bugs in the below line
  int x = a * 4;

  // There are potential integer overflow/underflow bugs in the below line
  long y = a * 10;

  long b = a;
  // There is no integer overflow/underflow bug in the below line
  long z = b * 10;

  printf("x: %d\n", x);
  printf("y: %lu\n", y);
  return 0;
}
