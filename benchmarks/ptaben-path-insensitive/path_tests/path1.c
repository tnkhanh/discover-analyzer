/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

void foo(int**, int*);

main(){
  int **x, *y;
  int  *c, *d, e, f;

  if(x)
    { x = &c; y = &e; }
  else
    { x = &d; y = &f; }

  foo(x,y);

  __assert_may_alias(c, &e);
  __assert_may_alias(d, &f);
  __assert_no_alias(c, &f);
  __assert_no_alias(d, &e);
}

void foo(int **p, int *q){
  *p = q;
}
