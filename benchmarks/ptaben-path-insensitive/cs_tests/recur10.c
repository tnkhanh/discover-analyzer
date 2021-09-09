/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int **p,*x, y, z;

void f() {
  if(z>5) return;
  z++;
  p = &x;
  if (1) {
    *p = &y;
    __assert_must_alias(x, &y);
    f();
    *p = &z;

    /// p doesn't point to x at the above line: although p's
    /// value changed by stmt "p=&x", the value flow can not
    /// reach "*p=&z" since it will flow into f() before
    /// "*p=&z" and connected with the entry of f(). So the
    /// store "*p=&z" can not be completed as p's pts is empty.

    // TRUNG: in the PTABen, it is originally MAYALIAS
    __assert_must_alias(x, &z);
    __assert_no_alias(x, &y);
    f();
  }
}

int main() {
  f();
  return 0;
}
