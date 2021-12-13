/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 30, 2020
 */

#include "discover.h"

int global_obj;
int *global_ptr = &global_obj;

class A {
  public:
    virtual void f(int *i) {
      __assert_no_alias(global_ptr, i);
    }
};

class B: public A {
public:
  virtual void f(int *i) {
      __assert_must_alias(global_ptr, i);
    }
};

int main(int argc, char **argv)
{
  int *ptr = &global_obj;

  A *pa = new B;

  if (B *pb = dynamic_cast<B*>(pa)) {
    pb->f(ptr);
  }

  return 0;
}
