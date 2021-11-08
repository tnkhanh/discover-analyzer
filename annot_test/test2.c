#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  // BUG: there is an integer overflow bug in the below line
  int x = /*@{Bug:IntegerOverflow,MemoryLeak , Something*/ a * 4 /*@:Bug}*/;

  // BUG: there is an integer overflow bug in the below line
  long y = /*@{Bug:IntegerOverflow , NullPointerDeref, Hehe*/ a * 10 /*@:Bug}*/;

  // SAFE: there is NO integer overflow bug in the below line
  // The conversion `long b = a` implies that `b * 10` is SAFE from integer overflow.
  long b = a;
  long z = /*@{Safe:IntegerOverflow*/ b * 10 /*@:Safe}*/;
  int t = z * 10;

  printf("x: %d\n", x);
  printf("y: %lu\n", y);
  printf("t: %d\n", t);
  return 0;
}
