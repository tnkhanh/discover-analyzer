#include <stdio.h>
#include <stdlib.h>

class A {
  public:
    int foo(int i) {
      return i + 1;
    }
    virtual int bar(int i) {
      return i + 2;
    }
};

int main() {
  A *a = new A;

  int x = a->foo(1);
  int y = a->bar(2);

  return 0;
}
