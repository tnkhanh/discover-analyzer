/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void foo(int**a, int*b){
	*a = b;
}

void main(){
	int *p,q,*x,y;

	foo(&p,&q);
  __assert_must_alias(p,&q);

	foo(&x,&y);
  __assert_must_alias(x,&y);
  __assert_no_alias(x,&q);
  __assert_no_alias(p,&y);
	*p = 100;
}
