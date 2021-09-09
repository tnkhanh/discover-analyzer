/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 20, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include "discover.h"

int global_obj_f;
int *global_ptr_f = &global_obj_f;

int global_obj_g;
int *global_ptr_g = &global_obj_g;

class A {
  public:
    virtual void f(int *i) {
    }
    virtual void g(int *i) {
      __assert_no_alias(global_ptr_f, i);
      __assert_must_alias(global_ptr_g, i);
    }
};

class B: public A {
  public:
    virtual void f(int *i) {
      __assert_must_alias(global_ptr_f, i);
      __assert_no_alias(global_ptr_g, i);
    }
    virtual void g(int *i) {
      __assert_no_alias(global_ptr_f, i);
      __assert_must_alias(global_ptr_g, i);
    }
};

int main()
{
  int *ptr_f = &global_obj_f;
  int *ptr_g = &global_obj_g;

  A *a = new B;
  a->f(ptr_f);
  a->g(ptr_g);

  // TRUNG: new code to cover all assertions
  A *p = new A;
  p->f(ptr_f);
  p->g(ptr_g);

  return 0;
}
