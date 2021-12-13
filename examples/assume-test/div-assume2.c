#include <stdio.h>
#include <discover.h>

int main() {
  int x;
  scanf("%d", &x);

  int z = x / 200000000;
  __assume_range(z, -5, 20);

  /* expects z range [-5, 10] */

  printf("%d\n", z);
	return 0;
}
