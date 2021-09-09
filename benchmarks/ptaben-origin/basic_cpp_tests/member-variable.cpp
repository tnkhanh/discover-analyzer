/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 11, 2020
 */

#include "discover.h"

int global_obj_a;
int *global_ptr_a = &global_obj_a;

int global_obj_b;
int *global_ptr_b = &global_obj_b;

class A {
  public:
    virtual void f(int *i) {
      __assert_must_alias(global_ptr_a, i);
      __assert_no_alias(global_ptr_b, i);
    }
};

class B {
  public:
    B(A *a): _a(a) {}
    virtual void f(int *i) {
      __assert_no_alias(global_ptr_a, i);
      __assert_must_alias(global_ptr_b, i);
    }
    A *_a;
};

int main(int argc, char **argv)
{
  int *i = &global_obj_a;
  int *j = &global_obj_b;

  A *a = new A;
  B *b = new B(a);

  b->f(j);

  b->_a->f(i);

  return 0;
}
