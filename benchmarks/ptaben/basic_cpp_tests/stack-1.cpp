/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
 */

#include "discover.h"
#include <iostream>
#include <stack>

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

  stack<A> astack;
  A a;
  astack.push(a);

  const A *aptr = &(astack.top());
  astack.pop();
  aptr->f(ptr);

  return 0;
}
