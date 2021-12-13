/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int *x,y,z;

void f();

int main(){
  f();
  *x=100;
}

void f(){

  if(z){
    x = &y;
    f();
    x = &z;
    f();

    __assert_must_alias(x, &z);
    __refute_no_alias(x, &y);
  }

  __assert_may_alias(x, &z);
  __assert_may_alias(x, &y);
}
