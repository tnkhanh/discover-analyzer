/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
 */

#include "discover.h"

int global_int_obj;
int *global_int_ptr = &global_int_obj;


float global_float_obj;
float *global_float_ptr = &global_float_obj;


class A {
  public:
  virtual void f(int *i) {
    __assert_must_alias(global_int_ptr, i);
    __assert_no_alias(global_float_ptr, i);
  }
  virtual void g(float *j) {
    __assert_no_alias(global_int_ptr, j);
    __assert_must_alias(global_float_ptr, j);
  }
};

class B: virtual public A {
  public:
  virtual void f(int *i) {
    __assert_must_alias(global_int_ptr, i);
    __assert_no_alias(global_float_ptr, i);
  }
  virtual void g(float *j) {
    __assert_no_alias(global_int_ptr, j);
    __assert_must_alias(global_float_ptr, j);
  }
};

int main(int argc, char **argv)
{
  int *i = &global_int_obj;
  float *j = &global_float_obj;

  A *p = new B;

  p->f(i);
  p->g(j);

  return 0;
}
