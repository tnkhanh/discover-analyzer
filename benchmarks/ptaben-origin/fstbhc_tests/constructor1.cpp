/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int *global = nullptr;

class A {
public:
  int x;

  A() {
    foo();
  }

  virtual void foo() {
    global = &this->x;
  }
};

class B : public A {
public:
  B() { }

  // Never called.
  virtual void foo() override {
    global = nullptr;
  }
};

int main(void) {
  B *b = new B();
  __assert_may_alias(global, &b->x);
}
