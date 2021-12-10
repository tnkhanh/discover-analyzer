#include <stdio.h>
#include <discover.h>

int main() {
  int x, y;
  scanf("%d%d", &x, &y);

  __assume_range(x, -8, 8);
  int z = x / y;

  printf("%d\n", z);
	return 0;
}
