/*
 * Branches for testing flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 08/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int main() {
	int **p, **q;
	int *x, *y;
	int x0, y0;
	if (x0) {
		p = &x;
		q = &y;
		__assert_no_alias(p, q);
	}
	else {
		p = &y;
		q = &x;
		__assert_no_alias(p, q);
	}

  // TRUNG: previously MAYALIAS in PTABen
	__assert_no_alias(p, q);
	return 0;
}
