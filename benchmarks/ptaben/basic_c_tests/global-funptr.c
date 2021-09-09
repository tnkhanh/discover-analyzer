/*
 * Global variables with function pointer initialisation.
 * Author: Sen Ye
 * Date: 07/05/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Aug 27, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int x, y;
int* p;

void foo() {
	p = &y;
}

struct MyStruct {
	void (*fp)();
	int* f1;
};

struct MyStruct context = { foo, &x };

int main()
{
	(*context.fp)();
	int* q = p;
	__assert_must_alias(q, &y);
	return 0;
}
