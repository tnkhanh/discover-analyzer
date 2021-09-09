/*
 * Function pointer.
 * Author: Sen Ye
 * Date: 10/10/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void func1(int *p, int *q) {
  // if function pointer solved correctly,
  // p and q will alias due to CS1
  __assert_must_alias(p,q);
  *p = *q;
}

void (*fp)(int*,int*);

int main() {
  int x, y;
  int *m, *n;
  if (x) {
    m = &x, n = &x;
    fp = func1;
    fp(m,n); // CS1
  }
  else {
    m = &x; n = &y;
//    func1(m,n); // CS2
  }
  return 0;
}
