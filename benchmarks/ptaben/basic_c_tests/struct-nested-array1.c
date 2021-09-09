/*
 * Struct with array of structs.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Sep 20 , 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct InnerArrayStruct {
	int f1[100];
};

struct ArrayStruct {
	int f1;
	char f2;
	struct InnerArrayStruct f3;
	int f4;
};

int main() {
	struct ArrayStruct* p;
	struct ArrayStruct s;
	int* q;

	p = &s;
	q = &s.f3.f1[40];

  // TRUNG: originally MayAlias in PTABen
	__assert_no_alias(&p->f3.f1[10], q);

  // TRUNG: originally MayAlias in PTABen
	__assert_no_alias(&p->f3.f1[20], &p->f3.f1[30]);

	__assert_no_alias(&p->f3.f1[0], &s.f4);

	return 0;
}
