#include <stdio.h>
#include <discover.h>

int main(int argc, char** argv) {
  int a = 1;

  printf("Input an integer: ");
  scanf("%d", &a);
  
  if (a > 10) a = 1234567890;
  short x = /*@{Bug:IntegerOverflow*/ a * 10 /*@:Bug}*/;

  printf("Input an integer: ");
  scanf("%d", &a);

  if (a > 10) a = 1234560;

  x = /*@{Bug:IntegerOverflow*/ a * 10 /*@:Bug}*/;

  printf("%d\n", x);

  return 0;
}
