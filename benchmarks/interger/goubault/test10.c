#include <discover.h>

int main()
{
  int i;
  int j;
  int k;
  int l;

  i = 0;
  k = 9;
  j = -100;
  l = 0;
  while (l <= 1000) {
    while (i <= 100) //1
    {
      i = i + 1; //2
      __assert_range_full(i,1,101);
      while (j < 20){ //3
        j = i+j; //4 //5
        __assert_range_full(j,-99,20);
      }
      k = 4; //6
      __assert_range_full(k,4,4);
      while (k <=3){ //7
        k = k+1; //8 //9
        __assert_range_upper_bound(k,4);
      }
    } //10
    l = l+j;
    __assert_range_full(l,20,1020);
  }
}



