// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-01
//
// We assume sizeof(int)=4.

#include <stdio.h>
#include <limits.h>
//#include "discover.h"

int main() {
	unsigned int maxUInt = UINT_MAX;
	unsigned int x = (maxUInt / 2 );
  int y = /*@{Bug:IntegerOverflow*/x + 1/*@:Bug}*/;
  printf("%u\n", x);
	printf("%d\n", y);
	return 0;
}
