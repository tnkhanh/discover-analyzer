/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void f(int** a, int *b) {
  *a = b;
}

typedef void (*fp)(int**,int*);

void main()
{
  int **x,*y,*z;
  int *m,*n,m1,n1;
  m = &m1;
  n = &n1;
  fp p = &f;
  x = &y;
  (*p)(x,m);       /* these are equivalent */
  __assert_must_alias(y,m);
  __assert_no_alias(y,n);
  x = &z;
  p(x,n);
  __assert_must_alias(z,n);
  __assert_no_alias(z,m);
}
