/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int **p,*q;
  int *a,*b,c,d,e;
  q = &c;

  if(a){
    p = &a;
    if(c){
      q = &d;
    }
  }
  else{
    p = &b;
    q = &e;
  }

  *p = q;
  __assert_may_alias(a, &c);
  __assert_may_alias(a, &d);
  __assert_may_alias(b, &e);
  __assert_no_alias(a, &e);
  __assert_no_alias(b, &c);
  __assert_no_alias(b, &d);
}
