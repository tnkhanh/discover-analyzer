/*
 * Cycle
 * Author: Sen Ye
 * Date: 11/10/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: May 24, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main() {
	int **x1, **y1, **z1;
	int *x2, *y2, *z2, *y2_;
	int x3, y3, z3, y3_;

	x2 = &x3, y2 = &y3, z2 = &z3;
	x1 = &x2, y1 = &y2, z1 = &z2;

	if (y3_) {
		y1 = &y2_;
		y2_ = &y3_;
	}

	*x1 = *y1;
	*y1 = *z1;
	*z1 = *x1;

  __assert_no_alias(x2, y2);

	__assert_must_alias(z2, x2);

	return 0;
}
