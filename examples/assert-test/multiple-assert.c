#include <stdio.h>

int main() {
  int maxInt = 2147483647;
	int u = /*@{Bug:IntegerOverflow*/(maxInt + 1)/*@:Bug}*/ - 23;
	printf("%d\n", u);

  int y = 2147483647;
	long x = /*@{Bug:IntegerOverflow*/y + 1/*@:Bug}*/;
	printf("%ld\n", x);

	int z = -2147483647 - 1;
	/*@{Bug:IntegerOverflow*/z--;/*@:Bug}*/
	printf("%d\n", z);
	return 0;
}
