/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int a;

int *foo(int *x){
  int* z = x;
  int* y;
  if(x)
    y = foo(z);
  else
    y = x;

  __assert_may_alias(y, &a);

  return y;
}

int main(){
  int* p;
  p = &a;
  foo(p);
}
