/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 28, 2020
 */

#include "discover.h"

int global_obj;
int *global_ptr = &global_obj;

class A;

void g(A *p);

class A {
  public:
    A(int *i): aptr(i) {}
    virtual ~A() { g(this); }
    virtual void f() { __assert_must_alias(global_ptr, aptr); }
  private:
    int *aptr;
};

class B: public A {
  public:
    B(int *i): A(i), bptr(i) {}
    virtual ~B() { g(this); }
    virtual void f() { __assert_must_alias(global_ptr, bptr); }
  private:
    int *bptr;
};

void g(A *p) {
  p->f();
}

int main(void)
{
  int *i = &global_obj;

  B *b = new B(i);

  delete b;

  return 0;
}
