/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int **p, **q;
  int *a, *b;
  int *m, *n;
  int a1, b1, m1;

  a = &a1;
  b = &b1;
  m = &m1;
  p = &a;
  q = &b;

  if (a) {
    p = &b;
    q = &a;
  }

  *p = m;
  n = *q;

  __assert_may_alias(p, &a);
  __assert_may_alias(p, &b);
  __assert_may_alias(n, &a1);
  __assert_may_alias(n, &b1);
  __assert_no_alias(p, q);
  __assert_no_alias(n, &m1);
}
