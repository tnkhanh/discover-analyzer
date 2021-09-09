/*
 * Alias due to lack of path-sensitivity.
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: May 13, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main()
{
  int *p, *q;
  int a, b, c;
  if (c) {
    p = &a;
    q = &b;
  }
  else {
    p = &b;
    q = &c;
  }

  __assert_may_alias(p, q);
}
