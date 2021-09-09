/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){

  int **p,*q;
  int *b,*c,d,e;

  p = &c;
  q = &e;

  if(d){
    p = &b;
  }
  else{
    q = &d;
  }

  *p = q;

  __assert_may_alias(b,&e);
  __assert_may_alias(c,&d);
  __assert_no_alias(c,&e);
  __assert_no_alias(b,&d);
}
