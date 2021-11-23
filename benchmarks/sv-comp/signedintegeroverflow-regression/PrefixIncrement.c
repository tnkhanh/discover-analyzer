// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-09
//
// We assume sizeof(int)=4.

#include <stdio.h>
//#include "discover.h"

int main() {
	int x = 2147483647;
	/*@{Bug:IntegerOverflow*/++x;/*@:Bug}*/
	printf("%d\n", x);
	return 0;
}
