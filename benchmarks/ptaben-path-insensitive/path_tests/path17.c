/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int **p,*q;
  int *a,*b,c,d;
  int *x;

  p = &a;
  if(p){
    if(c){
      q = &c;
    }
  }
  else{
    if(c)
      p = &b;
    q = &d;
  }
  if(p){
    *p = q;
  }

  __assert_may_alias(a,&d);
  __assert_may_alias(a,&c);
  __assert_may_alias(b,&d);
  __assert_no_alias(b,&c);
}
