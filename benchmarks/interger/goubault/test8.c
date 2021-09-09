#include <discover.h>

void main() {
  int i;
  int j;
  int k;
  i = 0;
  j = 0;
  k = 0;
  while (i < 100) {
    while (j < 100) {
      while (k < 100) {
	      i=i+1;
        __assert_range_full(i,1,100);
	      j=j+1;
        __assert_range_full(j,1,100);
	      k=k+1;
        __assert_range_full(k,1,100);
      }
    }
  }
}
