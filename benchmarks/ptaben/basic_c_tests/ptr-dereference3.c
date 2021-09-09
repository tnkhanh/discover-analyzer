/*
 * Migrated to Discover: Ta Quang Trung
 * Date: June 05, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

void foo(int q){
  int i = 10;
  int k = i;
}

int main(){
  int *s,*r,***x,**y,t,z,k;
  s = &t;
  r = &z;
  y = &r;
  s = r;
  __assert_must_alias(s,&z);
  x = *y;
  __assert_must_alias(x,r);
  foo(k);
  return 0;
}
