#include <discover.h>

int main() {
  int i;
  int j;
  int k;
  i=0;
  j=100;
  k=1;
  while (i<j) {
    k=k+1;
    __assert_range_lower_bound(k,2);
  }
}
