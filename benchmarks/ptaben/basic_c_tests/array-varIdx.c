/*
 * Alias with array
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 30, 2020
*/

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct MyStruct {
	int * f1;
	int * f2;
};

int main() {
	struct MyStruct s[2];
	int a, b;
	s[0].f1 = &a;
	s[1].f1 = &b;

	// Different fields of different elements in a
	// certain array are treated as different objects.
	__assert_no_alias(s[a].f1, s[b].f2);
	__refute_must_alias(s[a].f1, s[b].f2);

	return 0;
}
