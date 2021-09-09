#include <discover.h>

int main() {
  int i;
  int j;

  i=0;
  j=10;
  while (i<=j) 
  {
    while (j>=5) 
    {
      if (i==2){
	        i=3;
          __assert_range_full(i,3,3);
      }
      if (i==5){
	        i=4;
          __assert_range_full(i,4,4);
      }
      j=-1+j;
      __assert_range_full(j,4,9);
    }
    i=i+1;
    //res [1,11]
    __assert_range_full(i,1,5);
  }
}
