/*
 * Alias due to casting between
 * integer and pointer.
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Sep 20 , 2020
*/

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct MyStruct {
	int f1;
	int f2;
};

int main() {
	struct MyStruct ms;
	int *p, *q;
	p = &(ms.f1);
	// cast pointer to integer
	int addr = (int)p;
	// cast integer to pointer and
	// q would point to blackhole
	q = (int*)addr + 1;

	__refute_may_alias(p, q);
	__assert_no_alias(p, q);
	return 0;
}
