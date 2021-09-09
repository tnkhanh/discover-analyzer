#include <discover.h>

int main() {
  int i;
  int j;
  int k;
  i=1;
  k=0;
  while (k<1000) {
    while (i<100) 
      {
	      j=100;
        while (j>1){
          j = j-i;
          __assert_range_full(j,-96,99);
        }
        i=i+1;
        __assert_range_full(i,2,100);
      }
    k=k-j;
    __assert_range_upper_bound(k,-1);
  }
}
