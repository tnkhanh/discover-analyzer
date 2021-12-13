/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int y,*q,*r,*f,*e,c,d;

void foo(int **p){
  if(y){
		p = &e;
		f = &y;
	}

	*p = f;
  __assert_may_alias(q, &d);
  __assert_may_alias(r, &c);
  __assert_may_alias(e, &y);
  __assert_no_alias(q, &y);
  __assert_no_alias(r, &y);
}

int main(){
	int **a, **b;
  e = 0;

	a = &q; f = &d;
	foo(a);

  b = &r; f = &c;
  foo(b);
  __assert_no_alias(r, &d);
  __assert_no_alias(q, &c);
  __assert_no_alias(e, &d);
  __assert_no_alias(e, &c);
}
