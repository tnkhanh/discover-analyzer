/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 01, 2020
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
  virtual void g(int *j) {
    __assert_no_alias(global_ptr_a, j);
    __assert_must_alias(global_ptr_b, j);
  }
};

class C: public A, public B {
  public:
  virtual void f(int *i) {
    __assert_must_alias(global_ptr_a, i);
    __assert_no_alias(global_ptr_b, i);
  }
  virtual void g(int *j) {
    __assert_no_alias(global_ptr_a, j);
    __assert_must_alias(global_ptr_b, j);
  }
};


int main(int argc, char **argv)
{
  int *i = &global_obj_a;
  int *j = &global_obj_b;

  A *pa;
  B *pb;
  C c;

  pa = &c;
  pa->f(i);

  pb = &c;
  pb->g(j);

  return 0;
}
