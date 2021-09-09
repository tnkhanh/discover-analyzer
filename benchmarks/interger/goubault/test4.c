#include <discover.h>

int main(int n) {
  int i,j,k,l,m;
  i = 0;
  j = 100;
  k = 1000;
  l = 10000;
  m = 100000;
  while (i < 1000){ 
    i = i+1;
    __assert_range_full(i,1,1000);   
  }
  while (j  < 1000){
    j = j+k;
    __assert_range_full(j,1100,1100);
  }
  while (k > 100){
    k = k-j;
    __assert_range_full(k,-100,-100);
  }
  while (l > 1000){
    l = l+k;
    __assert_range_full(l,1000,9900);
  }
  while (m > 1){
    m = m-l;   
    __assert_range_full(m,0,99000);
  }
}

