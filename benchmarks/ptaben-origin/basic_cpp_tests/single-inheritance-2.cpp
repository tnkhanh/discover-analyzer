/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
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

class B: public A {
  public:
    virtual void f(int *i) {
      __assert_no_alias(global_ptr_a, i);
      __assert_must_alias(global_ptr_b, i);
    }
};

int main(int argc, char **argv)
{
  int *i = &global_obj_a;
  A *pa = new A;
  pa->f(i);


  int *j = &global_obj_b;
  B *pb = new B;
  pb->f(j);

  return 0;
}
