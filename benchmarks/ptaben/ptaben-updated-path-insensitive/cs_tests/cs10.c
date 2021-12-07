/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int obj, t,s;
void foo(int**, int**);

void main() {
  int **x, **y;
  int *a, *b, *c, *d,*e;
  x = &a; y = &b;
  foo(x,y);

  __assert_must_alias(b, &obj);

  *b = 5;
  c = &s;

  if(t)
    { x = &c; y = &e; }
  else
    { x = &d; y = &d;}

  foo(x,y);

  __assert_may_alias(e, d);
  __assert_may_alias(e, &obj);

  *e = 10;
}

void foo(int **p, int **q){
  *q = &obj;
}
