/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 11, 2020
 */

#include "discover.h"
#include <iostream>
#include <queue>

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

  queue<A*> aqueue;
  A *a = new A;
  aqueue.push(a);

  const A *aptr = aqueue.front();
  aqueue.pop();
  aptr->f(ptr);

  return 0;
}
