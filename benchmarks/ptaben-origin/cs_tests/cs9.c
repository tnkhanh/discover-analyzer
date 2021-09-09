/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int obj, t, s;
int *k = &s;
void foo(int**, int**);

main(){
  int **x, **y;
  int *a, *b, *c, *d,*e;

  a = &t;
  x = &a; y = &b;
  foo(x,y);

  __assert_no_alias(a, b);
  __assert_must_alias(b, &obj);

  *b = 5;
  c = &t;
  c = &s;
  a = c;

  __assert_must_alias(a, c);

  if(t)
    { c = &obj; x = &c; y = &e; }
  else
    { x= &d; y = &d; }

  e = &t;
  foo(x,y);

  __assert_may_alias(c, d);
  __assert_may_alias(d, &obj);
  __assert_may_alias(a, c);
  __assert_no_alias(a, d);
  __assert_may_alias(c, d);

  *e = 10;
}

void foo(int **p, int **q){
  *p = *q;
  *q = &obj;
}
