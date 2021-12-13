/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void foo(int **p, int **q){
	*q = *p;
}


void main(){
	int **a,**b,**c,**d,**e,**f,*x,*y,*z,*w,*k,x1,y1,z1,w1,k1;
	x = &x1;
	y = &y1;
	w = &w1;
	k = &k1;

	a = &x;
	b = &y;
	c = &w;
	d = &k;

	foo(a,b);
  __assert_must_alias(x,y);
	foo(c,d);
  __assert_must_alias(w,k);
  __assert_no_alias(x,k);
  __assert_no_alias(y,w);
}
