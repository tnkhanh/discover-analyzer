#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  // BUG: there is an integer overflow bug in the below line
  int x = /*{bug:integer_overflow*/ a * 4 /*:bug}*/;

  // BUG: there is an integer overflow bug in the below line
  long y = /*{bug:integer_overflow*/ a * 10 /*:bug}*/;

  // SAFE: there is NO integer overflow bug in the below line
  // The conversion `long b = a` can guarantee that `b * 10` is SAFE.
  long b = a;
  long z = /*{safe:integer_overflow*/ b * 10 /*:safe}*/;

  printf("x: %d\n", x);
  printf("y: %lu\n", y);
  return 0;
}
