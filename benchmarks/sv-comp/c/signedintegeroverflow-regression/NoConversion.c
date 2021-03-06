// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-09
//
// We assume sizeof(int)=4 and sizeof(long)>4.

#include <stdio.h>
//#include "discover.h"

int main() {
	// a conversion to long after the operation does not save the operation
	// from producing an overflow
  int y = 2147483647;
	long x = /*@{Bug:IntegerOverflow*/y + 1/*@:Bug}*/;
	printf("%ld\n", x);
	return 0;
}
