// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-01
//
// We assume sizeof(int)=4.

#include <stdio.h>
//#include "discover.h"

int main() {
	int minInt = -2147483647 - 1;
	int x = /*@{Bug:IntegerOverflow*/(minInt + -1) /*@:Bug}*/ + 23;
	printf("%d\n", x);
	return 0;
}
