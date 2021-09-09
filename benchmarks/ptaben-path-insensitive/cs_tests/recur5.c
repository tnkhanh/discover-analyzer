/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int* x, x1;

void f(int **m){
  int **n,*y,*k,z,r;
  n = &y;
  y = &z;
  if(z==1){
    *n = &r;
    __assert_must_alias(y, &r);
    __refute_no_alias(y, &z);
    k = *n;
    __assert_must_alias(k, &r);
    __refute_no_alias(k, &z);
    f(n);
  }
}

int main(){
  x = &x1;
  f(x);
}
