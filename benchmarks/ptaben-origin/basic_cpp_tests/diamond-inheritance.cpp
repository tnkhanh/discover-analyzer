/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 30, 2020
 */

#include <stdio.h>
#include "discover.h"

int global_obj_b;
int *global_ptr_b = &global_obj_b;

int global_obj_c;
int *global_ptr_c = &global_obj_c;

class A {
  public:
    virtual void f(int *i) {
      __assert_no_alias(global_ptr_b, i);
      __assert_no_alias(global_ptr_c, i);
    }
};

class B: public A {
  public:
    virtual void f(int *i) {
      __assert_must_alias(global_ptr_b, i);
      __assert_must_alias(global_ptr_c, i);
    }
};

class C: public A {
  public:
    virtual void f(int *i) {
      __assert_must_alias(global_ptr_b, i);
      __assert_must_alias(global_ptr_c, i);
    }
};

class D: public B, public C {
};

int main()
{
  int *ptr_b = &global_obj_b;
  int *ptr_c = &global_obj_c;

  D d;
  ////// if D has not implemented its own f(), this will cause an error
  //d.f();

  /////// error
  //A *a = &d;
  //a->f();

  B *b = &d;
  b->f(ptr_b);

  C *c = &d;
  c->f(ptr_c);

  return 0;
}
