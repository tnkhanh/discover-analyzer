#include <stdexcept>
#include "discover.h"

int* foo(int* a, int* b) {
  if( *b == *a ) {
    throw b;
  }
  return a;
}

int main () {
  int a, b;
  int *x, *y, *z;

  a = 0;
  b = 1;

  x = &a;
  y = &b;

  try {
    z = foo(x, y);
  } catch (int* w) {
    z = w;
  }

  // __assert_no_alias(z, x);
  __assert_may_alias(z, x);
  __assert_may_alias(z, y);
  // __assert_no_alias(z, y);

  return 0;
}
