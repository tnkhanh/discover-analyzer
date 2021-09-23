#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  int x = a * 4;
  __assert_integer_overflow(&x, sizeof(x));

  long y = a * 10;
  __assert_no_integer_overflow(&y, sizeof(y));

  printf("x: %d\n", x);
  printf("y: %lu\n", y);
  return 0;
}
