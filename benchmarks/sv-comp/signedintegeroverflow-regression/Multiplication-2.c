// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-01
//
// We assume sizeof(int)=4.

#include <stdio.h>
#include "discover.h"

int main() {
	int x = /*@{Bug:IntegerOverflow*/(65536 * 32768)/*@:Bug}*/ - 1;
	printf("%d\n", x);
	return 0;
}
