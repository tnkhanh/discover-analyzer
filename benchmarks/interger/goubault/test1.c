#include <discover.h>

void main() {
  int x;
  x=0;
  while (x<100) {
    x=x+1;
    __assert_range_full(x,1,100);
  }
}

