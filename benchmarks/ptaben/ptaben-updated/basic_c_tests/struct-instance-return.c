/*
 * Return a struct instance from function.
 * Author: Sen Ye
 * Date: 07/05/2014
  *
 * Migrated to Discover: Ta Quang Trung
 * Date: Sep 20 , 2020
*/

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct MyStruct {
	int* f1;
	char f2;
};

int x, y;

struct MyStruct foo() {
	struct MyStruct m;
	m.f1 = &x;
	return m;
}

int main() {
	struct MyStruct m;
	m = foo();

  // TRUNG: previously, it was EXPECTEDFAIL_MAYALIAS in PTABen
	__assert_must_alias(m.f1, &x);

	__assert_no_alias(m.f1, &y);
	return 0;
}
