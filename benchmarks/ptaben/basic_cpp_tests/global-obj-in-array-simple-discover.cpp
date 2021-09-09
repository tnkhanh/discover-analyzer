/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 31, 2020
 */

#include "discover.h"
#include <iostream>

using namespace std;

int globalObj = 10;
int *globalPtr = &globalObj;

class A {
public:
  A(int d): data(d) {}

  virtual void f(int *p) const {
    // cout << "data: " << data << "\n";
    __assert_must_alias(p, globalPtr);
  }

private:
  int data;
};

struct TableEntry {
  int num;
  const A *p;
};

int main(int argc, char **argv)
{

  A a1(1);


  TableEntry theTable[] = {
    {1, &a1}
  };


  TableEntry theEntry = theTable[0];
  const A *p = theEntry.p;

  p->f(globalPtr);

  return 0;
}
