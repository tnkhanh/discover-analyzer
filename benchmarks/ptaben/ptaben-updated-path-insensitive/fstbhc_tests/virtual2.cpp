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
    // TRUNG: previously, it was EXPECTEDFAIL_NOALIAS in PTABen
    __refute_no_alias(x, y);
  }
};

int main(void) {
  int i, j;

  A *a = new A();
  // A::foo::x == &i && A::foo::y == &j
  a->foo(&i, &j);

  B *b = new B();
  if (i) a = (A *)b;
  // A::foo::x == &i, A::foo::y == &j
  // B::foo::x == &i, B::foo::y == &j
  a->foo(&i, &j);
  // B::foo::x == &j, B::foo::y == &i
  // but from the previous call also...
  // B::foo::x == &i, B::foo::y == &j
  b->foo(&j, &i);
}
