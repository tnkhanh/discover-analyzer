/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int x,y,*q,*f,*e,d;

void foo(int **p){
  f = &x;

  if (x) {
    p = &e;
    f = &y;
  }

  *p = f;
  __assert_may_alias(q, &x);
  __assert_may_alias(e, &y);
  __assert_no_alias(q, &y);
  __assert_no_alias(q, &d);
  __assert_no_alias(e, &x);
}

int main(){
  int **a, c;
  a = &q; f = &d;
  foo(a);
}
