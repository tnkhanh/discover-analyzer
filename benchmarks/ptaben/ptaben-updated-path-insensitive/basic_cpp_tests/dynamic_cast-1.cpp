/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 30, 2020
 */

#include "discover.h"

int global_obj;
int *global_ptr = &global_obj;

int global_obj2;
int *global_ptr2 = &global_obj2;

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
  int *ptr2 = &global_obj2;

  A *pa = new B;

  if (B *pb = dynamic_cast<B*>(pa)) {
    pb->f(ptr);
  }

  // TRUNG: new code to cover all assertions
  A *qa = new A;
  qa->f(ptr2);

  return 0;
}
