/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void foo(int **x,int**y,int **z, int *w){
  int *t;
  *y = w;
  t = *x;
  *z = t;
}

int main(){
  int *a,*b,*c,d,*p,*q,r,a1;
  a = &a1;
  foo(&a,&b,&c,&d);
  __assert_must_alias(b,&d);
  __assert_must_alias(c,&a1);
  foo(&p,&p,&q,&r);
  __assert_must_alias(p,&r);
  __assert_must_alias(p,q);
  __assert_no_alias(b,p);
  __assert_no_alias(c,p);
}
