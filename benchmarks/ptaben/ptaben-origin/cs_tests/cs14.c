/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void foo(int*,int*);

main(){
  int*x, *y;
  int a,b, c;
  if(c)x=&a;
  else x=&b;
  __assert_may_alias(x,&a);
  __assert_may_alias(x,&b);
  foo(x,y);
  x = &c;
  foo(x,y);
  __assert_must_alias(x,&c);
  __assert_no_alias(x,&a);
  __assert_no_alias(x,&b);
}

void foo(int *p,int*q){
  *p = 100;
}
