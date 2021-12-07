/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"
#include "stdlib.h"

int *alloc( int size){
	return malloc(1);
}

void foo(int **p){
	*p = alloc(1);
	//*p = alloc();
}

void main(){
	int *a,*b,*c;
	foo(&a);
	foo(&b);
	foo(&c);
  __assert_no_alias(a,b);
  __assert_no_alias(b,c);
  __assert_no_alias(a,c);
}
