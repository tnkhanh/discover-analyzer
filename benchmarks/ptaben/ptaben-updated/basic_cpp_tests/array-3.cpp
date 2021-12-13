/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 28, 2020
 */

#include "discover.h"
#include <array>

using namespace std;

int global_obj;
int *global_ptr = &global_obj;

class A {
  public:
    virtual void f(int *i) const {
      __assert_must_alias(global_ptr, i);
    }
};

int main(int argc, char **argv)
{
  int *ptr = &global_obj;

  array<const A *, 2> aarray;
  A *a0 = new A;
  A *a1 = new A;

  aarray[0] = a1;
  aarray[1] = a1;

  const A *aptr = aarray.back();

  aptr->f(ptr);

  return 0;
}
