#include <stdio.h>

int main() {
  int x = -2147483647 - 1, y = 1;
  int z = x / y;
  printf("%d\n", z);

  scanf ("%d%d", &x, &y);
  z = x / y;
  printf("%d\n", z);
	return 0;
}
