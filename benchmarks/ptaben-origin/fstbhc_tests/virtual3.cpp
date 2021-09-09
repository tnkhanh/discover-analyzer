 /*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

// Tests virtual calls in constructors.

// A::A() calls foo(&i, &j); *must* be A::foo.
// B::B() calls foo(&j, &i); *must* be B::foo.

int i, j;

class A {
public:
  A() {
    foo(&i, &j);
  }

  virtual void foo(int *x, int *y) {
    __assert_no_alias(x, y);
  }
};

class B : public A {
public:
  B() {
    foo(&j, &i);
  }

  virtual void foo(int *x, int *y) override {
    __assert_no_alias(x, y);
  }
};

int main(void) {
  B *b = new B();
}
