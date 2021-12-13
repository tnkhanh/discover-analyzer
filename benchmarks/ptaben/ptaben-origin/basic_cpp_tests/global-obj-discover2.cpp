/*
 * Author: Ta Quang Trung
 * Date: October 31, 2020
 */

#include "discover.h"
#include <iostream>

using namespace std;

int globalObj = 10;
int *globalPtr = &globalObj;

class A {
public:
  virtual void f(int *p) const {
    __assert_must_alias(p, globalPtr);
  }
};

int main(int argc, char **argv)
{

  A a;

  const A *p = &a;
  p->f(globalPtr);

  return 0;
}
