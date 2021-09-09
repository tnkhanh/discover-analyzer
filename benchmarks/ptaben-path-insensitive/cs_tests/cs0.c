/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int* foo(int* x){
   return x;
}

int main(){

  int *x,*y,*p,*q,a,b;
  p = &a;
  q = &b;
  x = foo(p);
  y = foo(q);
  __assert_must_alias(x,p);
  __assert_must_alias(y,q);
  __assert_no_alias(x,q);
  __assert_no_alias(y,p);

}
