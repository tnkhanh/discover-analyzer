// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-06
//
// We assume sizeof(int)=4.

#include <stdio.h>
//#include "discover.h"

int main() {
	int x = /*@{Safe:IntegerOverflow*/(65536 * -32768)/*@:Safe}*/ ;
	printf("%d\n", x);
	return 0;
}
