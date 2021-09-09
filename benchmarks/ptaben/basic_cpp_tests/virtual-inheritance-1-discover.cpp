/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
 */

#include "discover.h"
#include <stdio.h>

int global_obj;
int *global_ptr = &global_obj;

class A {
  public:
    virtual void f(int *i) {
      __assert_must_alias(global_ptr, i);
    }
};

class B: virtual public A {
};


int main(int argc, char **argv)
{
  int *ptr = &global_obj;

  B *pb = new B;
  pb->f(ptr);

  return 0;
}
