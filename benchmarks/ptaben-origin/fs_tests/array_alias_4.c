/*
 * Alias with array
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

struct MyStruct {
  int * f1;
  int * f2;
};

int main() {
  struct MyStruct s[2];
  int a,b,c,d;
  s[0].f1 = &a, s[0].f2 = &c;//, s[0].f3 = &x;

  if (a)
    s[1].f1 = &c;

  __assert_may_alias(s[0].f1, s[1].f2);
  return 0;
}
