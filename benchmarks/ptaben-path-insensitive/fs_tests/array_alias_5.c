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
  struct MyStruct s[3];
  int a,b,c,d;
  s[0].f1 = &a, s[0].f2 = &c;//, s[0].f3 = &x;
  s[1].f1 = &b, s[1].f2 = &d;//, s[1].f3 = &y;

  if (a)
    s[1].f1 = &c;

  // TRUNG: previously MAYALIAS in PTABen
  __assert_no_alias(s[0].f1, s[1].f2);

  return 0;
}
