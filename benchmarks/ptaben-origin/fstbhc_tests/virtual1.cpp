/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

class A {
public:
  virtual void foo(int *x, int *y) {
    __assert_no_alias(x, y);
  }
};

class B : public A {
public:
  virtual void foo(int *x, int *y) override {
    __assert_no_alias(x, y);
  }
};

int main(void) {
  int i, j;

  A *a = new A();
  // A::foo::x == &i && A::foo::y == &j
  a->foo(&i, &j);

  B *b = new B();
  // B::foo::x == &j, B::foo::y == &i
  // NOALIAS stands because call resolution should
  // resolve to a single function.
  b->foo(&j, &i);
}
