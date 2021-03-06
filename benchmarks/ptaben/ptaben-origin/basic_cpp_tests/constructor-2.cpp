/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 28, 2020
 */

#include "discover.h"

int global_obj;
int *global_ptr = &global_obj;

class A;

void g(A *p, int *i);

class A {
  public:
    A(int *i) { g(this, i); }
    virtual void f(int *i) { __assert_must_alias(global_ptr, i); }
};

class B: public A {
  public:
    B(int *i): A(i) { g(this, i); }
    virtual void f(int *i) { __assert_must_alias(global_ptr, i); }
};

void g(A *p, int *i) {
  p->f(i);
}

int main(int argc, char **argv)
{
  int *i = &global_obj;

  B b(i);

  return 0;
}
