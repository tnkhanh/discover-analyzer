#include <discover.h>

void main(int n) {
  int i;
  int j;
  i=1;
  j=10;
  while (j >= i) {
    i = i+2;
    j = -1+j;
    printf("i: %d, j: %d\n", i, j);
    __assert_range_full(i,3,9);
    __assert_range_full(j,6,9);
  }
}
