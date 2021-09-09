/*
 * Struct alias in flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 08/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

struct MyStruct {
	int *f1;
	int *f2;
};

int main() {
	struct MyStruct s1, s2;
	int x, y;
	s1.f1 = &x;
	s1.f2 = &y;
	s2.f1 = &y;
	s2.f2 = &x;
	__assert_no_alias(s1.f1, s1.f2);
	__assert_no_alias(s1.f1, s2.f1);

	s1.f1 = &y;
	__assert_must_alias(s1.f1, s2.f1);
	return 0;
}
