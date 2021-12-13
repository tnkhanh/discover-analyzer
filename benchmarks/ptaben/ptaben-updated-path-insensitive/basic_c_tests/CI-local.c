/*
 * Alias due to context-insensitive
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
  // Trung: in the original PTABen, the following was MAYALIAS
  __assert_no_alias(m,n);
}

int main()
{
  int *p, *q;
  int a,b;
  if (a) {
    p = &a;
    q = &b;
    foo(p,q);
  }
  else {
    p = &b;
    q = &a;
    foo(p,q);
  }
  return 0;
}
