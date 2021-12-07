/*
 * Branches for testing flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 08/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int main() {
  int *p, *q;
  int x, y;
  if (x)
    p = &x;
  else
    p = &y;
  q = &y;


  __assert_may_alias(p, q);
  return 0;
}
