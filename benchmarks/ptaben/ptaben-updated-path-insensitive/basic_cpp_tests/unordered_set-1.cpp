/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 11, 2020
 */

#include "discover.h"
#include <iostream>
#include <unordered_set>

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

  unordered_set<const A*> aset;
  A *a = new A;

  aset.insert(a);

  unordered_set<const A*>::iterator it = aset.begin();
  const A *aptr = *it;

  aptr->f(ptr);

  return 0;
}
