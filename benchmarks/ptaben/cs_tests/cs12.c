/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int obj;
void foo(int**, int**);

main(){
	int **x, **y;
	int *a, *b, *c, *d,*e;
	x=&a; y =&b;
	foo(x,y);
  __assert_must_alias(a,&obj);
	x = &c;
	foo(x,y);
  __assert_must_alias(c,&obj);
}

void foo(int **p, int **q){
	*p = &obj;
}
