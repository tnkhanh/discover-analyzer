#include <stdexcept>
#include "discover.h"

int* foo(int* a, int* b) {
  if( *b == *a ) {
    throw 1;
  }
  else if( *b > *a + 20) {
    throw b;
  }
  else if( *b > *a + 20) {
    throw a;
  }
  else if (*b > *a) {
    char* msg = "ABC";
    throw msg;
  }
  return a;
}

// int* bar(int* a, int* b) {
//   return foo(a, b);
// }

int main () {
  int a, b;
  int *x, *y, *z;

  a = 0;
  b = 1;

  x = &a;
  y = &b;

  try {
    z = foo(x, y);
  }
  catch (int i) {
    a = i;
    z = y;
  }
  catch (int* w) {
    z = w;
  }
  catch (char* msg) {
    z = y;
  }

  // __assert_no_alias(z, x);
  // __assert_may_alias(z, y);
  __assert_no_alias(z, y);

  return 0;
}
