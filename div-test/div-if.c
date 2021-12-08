#include <stdio.h>

int main() {
  int x, y;
  scanf("%d%d", &x, &y);

  if (x!=8) x = -8;
  if (y!=4) y = -4;

  int z = x / y;

  printf("%d\n", z);
	return 0;
}
