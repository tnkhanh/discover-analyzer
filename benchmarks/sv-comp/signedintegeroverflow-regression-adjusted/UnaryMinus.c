// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-09
//
// We assume sizeof(int)=4.

#include <stdio.h>
//#include "discover.h"

int main() {
	int minInt = -2147483647 - 1;
	int y = /*@{Bug:IntegerOverflow*/-minInt/*@:Bug}*/ - 23;
	printf("%d\n", y);
	return 0;
}
