/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 11, 2020
 */

#include "discover.h"
#include <list>
#include "list.cc" // TRUNG: add list.cc to generate IR for missing list functions

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

  list<const A*> alist;
  A a;
  alist.push_back(&a);

  const A *aptr = alist.front();

  aptr->f(ptr);

  return 0;
}
