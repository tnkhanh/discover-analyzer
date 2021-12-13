/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
 */

#include "discover.h"

#define LEN 100

int global_obj_f;
int *global_ptr_f = &global_obj_f;

int global_obj_g;
int *global_ptr_g = &global_obj_g;

class A {
  public:
    virtual void f(int *i) {
      __assert_must_alias(global_ptr_f, i);
      __assert_no_alias(global_ptr_g, i);
    }
    virtual void g(int *i) {
      __assert_no_alias(global_ptr_f, i);
      __assert_must_alias(global_ptr_g, i);
    }
    double *f1;
    int *f2;
    int *f3;
};

int main(int argc, char** argv)
{
  int *ptr_f = &global_obj_f;
  int *ptr_g = &global_obj_g;

  A a_array[LEN];
  A *pa;

  pa = a_array;
  for (int i = 0; i < LEN/2; ++i)
    pa += 1;

  //__assert_no_alias(&(pa->f2), &(pa->f3));

  pa->f(ptr_f);
  pa->g(ptr_g);

  return 0;
}
