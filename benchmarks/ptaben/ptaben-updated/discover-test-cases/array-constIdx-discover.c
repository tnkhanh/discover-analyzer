/*
 * Alias with array
 * Author: Ta Quang Trung
 * Date: July 01, 2020
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
	int a,b;
	s[0].f1 = &a;
	s[1].f2 = &a;

	// Different fields of different elements in a
	// certain array are treated as different objects.

	__assert_must_alias(s[0].f1, s[1].f2);

	return 0;
}
