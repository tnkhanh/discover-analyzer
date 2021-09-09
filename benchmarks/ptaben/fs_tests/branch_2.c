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
	int *p, *q;
	int x, y;
	q = &y;
	if (x) {
		p = &x;
		__assert_no_alias(p, q);
	}
	else {
		p = &y;
		__assert_must_alias(p, q);
	}
	return 0;
}
