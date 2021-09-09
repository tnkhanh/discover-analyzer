/*
 * Simple program to test flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 08/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int main() {
  int **p, **q;
  int *x, *y;
  int x0, y0;
  p = &x;
  q = &y;
  *p = &x0;
  *q = &y0;
  __assert_no_alias(*p, *q);
  *p = *q;
  __assert_must_alias(*p, *q);
  return 0;
}
