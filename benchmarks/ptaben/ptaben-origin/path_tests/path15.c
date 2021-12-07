/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int **p, *q, **w, *v, *a, a1, q1;
  a = &a1;
  p = &a;
  q = &q1;
  w = 0;

  if (p)
    *p = q;
  else
    w = &a;

  v = *w;
  __assert_no_alias(v, &q1);
  __assert_may_alias(v, &a1);
}
