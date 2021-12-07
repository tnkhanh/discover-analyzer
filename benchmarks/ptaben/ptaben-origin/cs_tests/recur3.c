/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int x, *y, z;
void f(int **m);

void main(){
  int **a = &y;
  f(a);
}

void f(int **m) {
  if(x){
    *m = &x;
    __assert_must_alias(y, &x);
    __assert_no_alias(y, &z);
    f(m);
  }
  else{
    *m = &z;
    __assert_must_alias(y, &z);
    __assert_no_alias(y, &x);
  }
}
