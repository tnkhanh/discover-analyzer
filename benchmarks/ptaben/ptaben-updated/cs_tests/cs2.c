/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int obj;

void foo(int **p, int **q, int **r){
	*r = *p;
	*q = &obj;
}

void main(){
	int **a,**b,**c,**d,**e,**f,*x,*y,*z,*w,*k,x1,y1,z1,w1,k1;
	x = &x1;
	y = &y1;
	z = &z1;
	w = &w1;
	k = &k1;

	a = &x;
	b = &y;
	c = &z;

	if(a){
		d = &w;
		e = &w;
		f = &k;
	}

	foo(a,b,c);
  __assert_must_alias(x,z);
  __assert_must_alias(y,&obj);

	foo(d,e,f);
  __assert_no_alias(w,k);
  __assert_may_alias(w,&obj);
  __assert_no_alias(x,w);
  __assert_no_alias(z,k);
  __assert_may_alias(y,w);
}
