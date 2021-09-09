#include <discover.h>

int main() 
{
  int i;
  int j;
  i=1;
  while (i<100) 
  {
    j=100;
    while (j>1){
      j = j-i;
      // res[-97,99]
      __assert_range_full(j,-96,99);      
    }
    i=i+1;
    __assert_range_full(i,2,100);
  }
}
