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
  A(int d): data(d) {}

  virtual void f(int *p) const {
    cout << "data: " << data << "\n";
    __assert_must_alias(p, globalPtr);
  }

private:
  int data;
};

struct TableEntry {
  int num;
  const A *p;
};

A a1(1);
A a2(2);
A a3(3);

TableEntry theTable[] = {
  {1, &a1},
  {2, &a2},
  {3, &a3}
  // {0, 0}
};

int main(int argc, char **argv)
{

  TableEntry* theEntry = theTable;
  theEntry++;
  // const A *p = theEntry->p;
  // p->f(globalPtr);

  theEntry++;
  const A *q = theEntry->p;
  q->f(globalPtr);

  return 0;
}
