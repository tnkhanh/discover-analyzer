/*
 * Global variable
 * Author: Sen Ye
 * Date: 13/10/2013
 * Description: initialise global variables when declared
 *		and check alias in main function.
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 05, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int x;
int *p, *q;
int **pp = &p;
int **qq = &q;

void foo() {
	p = &x;
}

void bar() {
	q = &x;
}

int main() {
  // TRUNG: originally MayAlias in PTBench
	__assert_no_alias(*pp, *qq);
	foo();
	bar();
	__assert_must_alias(*pp, *qq);
	return 0;
}
