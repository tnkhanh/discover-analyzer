// Author: heizmann@informatik.uni-freiburg.de
// Date: 2015-09-09
//
// We assume sizeof(int)=4 and sizeof(long)>4.

#include <stdio.h>
//#include "discover.h"

int main() {
	// The operand 1LL has type long long. Due to the usual arithmetic conversions, 
	// 2147483647 is converted to long long before the addition, hence there is 
	// no overflow.
  int y = 2147483647;
	int x = /*@{Safe:IntegerOverflow*/ /*@{Safe:IntegerOverflow*/(y + 1LL)/*@:Safe}*/  - 23;/*@:Safe}*/
	printf("%d\n", x);
	return 0;
}
