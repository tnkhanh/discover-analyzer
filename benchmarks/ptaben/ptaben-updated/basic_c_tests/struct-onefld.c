/*
 * Struct with one field.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 06, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct IntStruct {
	int f1;
};

struct CharStruct {
	char f1;
};

int main() {
	struct IntStruct* pint1, *pint2;
	struct IntStruct s;
	pint1 = &s;
	pint2 = &s;
	__assert_must_alias(&pint1->f1, &pint2->f1);
	__assert_must_alias(&pint1->f1, &s.f1);

	struct CharStruct* qint1, *qint2;
	struct CharStruct t;
	qint1 = &t;
	qint2 = &t;
	__assert_must_alias(&qint1->f1, &qint2->f1);
	__assert_must_alias(&qint1->f1, &t.f1);

	return 0;
}
