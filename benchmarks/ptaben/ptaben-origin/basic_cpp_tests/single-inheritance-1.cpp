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
    // The following may alias also holds
    // if we use flow-insensitive Andersen's analysis
    // since the vtable vtableptrA stored in the object
    // is not strongly updated to be vtableptrB
    //__assert_must_alias(global_ptr, i);
    }
};

class B: public A {
    virtual void f(int *i) {
      __assert_must_alias(global_ptr, i);
    }
};

int main(int argc, char **argv)
{
  int *ptr = &global_obj;

  A *pb = new B;
  pb->f(ptr);

  return 0;
}
