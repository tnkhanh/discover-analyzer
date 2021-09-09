/*
 * Alias due to lack of context-sensitivity.
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: May 31, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

void foo(int *m, int *n)
{
  __assert_may_alias(m,n);
  int x, y;
  x = *n;
  y = *m;
  *m = x;
  *n = y;
}

int main()
{
  int *p, *q;
  int a, b, c;
  if (c) {
    p = &a;
    q = &b;
    foo(p,q);
  }
  else {
    p = &b;
    q = &c;
    foo(p,q);
  }
  return 0;
}
