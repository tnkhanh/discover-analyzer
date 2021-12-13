/*
 * Struct casting.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Sep 20 , 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct SrcStruct {
	int* f1;
	int* f2;
	char f3;
};

struct DstStruct {
	char f1;
	int* f2;
	int* f3;
};

int main() {
	struct DstStruct* pdst;
	struct SrcStruct* psrc;
	struct SrcStruct s;
	int x, y, z;

	psrc = &s;
	psrc->f1 = &x;
	psrc->f2 = &y;

	pdst = (struct DstStruct*)psrc;

  // TRUNG: originally MayAlias in PTABen
	__refute_must_alias(pdst->f2, &x);

  // TRUNG: originally MayAlias in PTABen
	__assert_must_alias(pdst->f2, &y);

	pdst->f3 = &z;
  // TRUNG: originally MayAlias in PTABen
	__refute_must_alias(psrc->f2, &z);

	return 0;
}
