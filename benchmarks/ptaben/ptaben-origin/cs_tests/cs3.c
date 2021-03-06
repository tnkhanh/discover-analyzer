/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void foo(int **p, int **q, int **r,int *s){
  *r = *p;
  *q = s;
}

void main(){
  int **a,**b,**c,**d,**e,**f,*x,*y,*z,*w,*g,*k,x1,y1,z1,w1,k1,g1;
  x = &x1;
  y = &y1;
  z = &z1;
  w = &w1;
  g = &g1;
  k = &k1;

  a = &x;
  b = &y;
  c = &z;

  foo(a,b,c,k);
  __assert_must_alias(x,z);
  __assert_must_alias(y,&k1);
  d = &w;
  e = &w;
  f = &g;
  foo(d,e,f,k);
  __assert_no_alias(w,g);
  __assert_must_alias(w,&k1);
  __assert_no_alias(x,w);
  __assert_no_alias(z,g);
  __assert_must_alias(w,y);
}
