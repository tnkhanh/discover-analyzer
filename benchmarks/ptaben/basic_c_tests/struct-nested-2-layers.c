/*
 * Struct with nested structs.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 29, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct InnerStruct {
	int in1;
	char in2;
};

struct MidStruct {
	int mid1;
	struct InnerStruct mid2;
	char mid3;
};

struct OuterStruct {
	struct MidStruct out1;
	char out2;
	struct InnerStruct out3;
	int out4;
};

int main() {
	struct OuterStruct* pout;
	struct MidStruct* pmid;
	struct MidStruct* ptmp;
	struct InnerStruct* itmp;
	struct InnerStruct* pin;
	struct OuterStruct s;

	pout = &s;
	pmid = &s.out1;
	ptmp = &pout->out1;
	__assert_must_alias(ptmp, pmid);
	__assert_must_alias(&(ptmp->mid2.in1), &(pmid->mid2.in1));
	__assert_must_alias(&(ptmp->mid2.in2), &(pmid->mid2.in2));

	pin = &s.out1.mid2;
	itmp = &pout->out1.mid2;
	__assert_must_alias(itmp, pin);
	__assert_must_alias(&(itmp->in1), &(pin->in1));
	__assert_must_alias(&(itmp->in2), &(pin->in2));

	__assert_no_alias(&pout->out3, pin);

	return 0;
}
