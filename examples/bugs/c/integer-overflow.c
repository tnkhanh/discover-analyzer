#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  int x = a * 4;

  long y = a * 10;

  printf("x: %d\n", x);
  printf("y: %lu\n", y);
  return 0;
}
