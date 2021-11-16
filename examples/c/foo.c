#include <stdio.h>
// #include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);

  if (a > 10) a = 1234567890;
  else a = 1234567891;
  long x = /*@{Bug:IntegerOverflow*/ a * 10 /*@:Bug}*/;

  printf("Input an integer: ");
  scanf("%d", &a);

  if (a > 10) a = 123456;
  else a = 123457;

  short y;
  /*@{Bug:IntegerOverflow*/ y = a * 10 /*@:Bug}*/;

  long z = x+y;

  return z;
}
