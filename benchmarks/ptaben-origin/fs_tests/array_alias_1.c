/*
 * Array alias in flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 08/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

struct MyStruct {
  int *f1;
  int *f2;
};

int main() {
  struct MyStruct s[2];
  int x, y;
  s[0].f1 = &x;
  s[1].f2 = &y;

  // Arrays are treated as a single element.
  // Different fields have its own points-to set.
  // Same fields have same points-to set, even they belong
  // to different elements.
  __assert_no_alias(s[0].f1, s[0].f2);
  __assert_no_alias(s[0].f1, s[1].f2);

  __assert_may_alias(s[0].f1, s[1].f1);

  s[0].f1 = &y;
  __assert_must_alias(s[0].f1, s[1].f2);
  return 0;
}
