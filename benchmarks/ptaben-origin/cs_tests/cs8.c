/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int obj1,obj2;

void foo(int **p, int **q){
  *p = &obj1;
  *q = &obj2;
}

int main(){
  int **a,**b,*x,*y,*z;

  if (a) {
    a = &x;
    b = &y;
  }
  else {
    a = &z;
    b = &z;
  }

  foo(a,b);

  __assert_may_alias(x, &obj1);
  __assert_may_alias(z, &obj1);
  __assert_may_alias(y, &obj2);
  __assert_may_alias(z, &obj2);
  __assert_no_alias(x, &obj2);
}
