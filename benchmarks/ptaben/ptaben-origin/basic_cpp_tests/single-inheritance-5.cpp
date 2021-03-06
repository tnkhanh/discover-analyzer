/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
 */

#include "discover.h"

int global_obj;
int *global_ptr = &global_obj;

class A {
  public:
    virtual void f(int *i) {
      __assert_may_alias(global_ptr, i);
    }
};

class B: public A {
    virtual void f(int *i) {
      __assert_must_alias(global_ptr, i);
    }
};

class C: public A {
    virtual void f(int *i) {
      // TRUNG: previously NOALIAS in PTABen
      // __assert_must_alias(global_ptr, i);
      __assert_must_alias(global_ptr, i);
    }
};

int main(int argc, char **argv)
{
  int *ptr = &global_obj;

  A *pb;

  if(ptr)
    pb = new C;
  else
    pb = new B;

  pb->f(ptr);

  return 0;
}
