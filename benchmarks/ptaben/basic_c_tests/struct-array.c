/*
 * Struct with array.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Sep 20 , 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct ArrayStruct {
	int f1;
	char f2;
	int f3[100];
	int f4;
};

int main() {
	struct ArrayStruct* p;
	struct ArrayStruct s;
	int* q;

	p = &s;
	q = &s.f3[40];

  // TRUNG: originally MayAlias in PTABen
	__assert_no_alias(&p->f3[10], q);

  // TRUNG: originally MayAlias in PTABen
	__assert_no_alias(&p->f3[20], &p->f3[30]);

  // TRUNG: originally MayAlias in PTABen
	__assert_no_alias(&s.f3[0], &s.f3[99]);

	__assert_no_alias(&p->f3[0], &s.f4);

	return 0;
}
