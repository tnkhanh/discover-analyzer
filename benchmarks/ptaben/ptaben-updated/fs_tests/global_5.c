/*
 * Global pointer in flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 08/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int **pp, **qq;
int *p, *q;
int x;

void foo() {
	pp = &p;
	p = &x;
}

void bar() {
	qq = &q;
	q = &x;
}

int main() {
	foo();
	bar();
	__assert_must_alias(*pp, *qq);
	return 0;
}
