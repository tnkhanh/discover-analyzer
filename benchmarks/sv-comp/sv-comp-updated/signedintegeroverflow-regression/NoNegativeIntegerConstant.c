// Author: heizmann@informatik.uni-freiburg.de
// Date: 2016-01-25
//
// We assume sizeof(int)=4 and sizeof(long)>4.

#include <stdio.h>
//#include "discover.h"

int main() {
	// No overflow here.
	// The expression "2147483648" is an integer constant whose type is long.
	// The expression "-2147483648" is the result of a unary minus operation, 
	//         the result has type long.
	// The expression "-2147483648 - 1" is the result of a binary minus 
	//         operation whose left operand and result have type long.
  long y = -2147483648;
	long x = /*@{Safe:IntegerOverflow*/y - 1/*@:Safe}*/;
	printf("%ld\n", x);
	return 0;
}
