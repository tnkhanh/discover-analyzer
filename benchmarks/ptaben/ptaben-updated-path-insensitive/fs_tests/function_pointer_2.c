/*
 * Function pointer.
 * Author: Sen Ye
 * Date: 10/10/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"


void func1(int **p, int **q) {
  *p = *q;
  __assert_must_alias(p, q);
}

void (*fp)(int**,int**);

int main() {
  int o1, o2;
  int *x, *y;
  int **m, **n;
  x = &o1;
  y = &o2;
  m = &x;
  n = &x;
  fp = func1;
  fp(m,n);
  __assert_no_alias(x, y);
  return 0;
}
