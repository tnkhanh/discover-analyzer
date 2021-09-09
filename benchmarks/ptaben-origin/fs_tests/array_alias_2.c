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
  float * f3;
};

int main() {
  struct MyStruct s[3];
  int * p[2];
  int a,b,c,d;
  float x,y;
  s[0].f1 = &a, s[0].f2 = &c, s[0].f3 = &x;
  s[1].f1 = &b, s[1].f2 = &d, s[1].f3 = &y;
  p[0] = &c, p[1] = &d;

  // Same fields of different elements in a certain
  // array are treated as one object.
  __assert_may_alias(s[0].f1, s[1].f1);
  __assert_may_alias(p[0], s[1].f2);
  __assert_may_alias(s[0].f3, &y);

  // Different fields of different elements in a
  // certain array are treated as different objects.
  __assert_no_alias(s[0].f1, s[1].f2);
  __assert_no_alias(p[1], s[1].f1);

  if (a)
    s[1].f1 = &c;
  __assert_may_alias(s[0].f1, s[1].f2);

  return 0;
}
