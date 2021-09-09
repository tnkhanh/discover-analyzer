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

c1:
  if(e){
    p = &b;
    q = &d;
  }
  else if(b) {
    q = &e;
  }
  else if(c){
    printf("dummy branch\n");
  }
  else{
    goto c1;
  }

  *p = q;
  __assert_may_alias(p,&c);
  __assert_may_alias(p,&b);
  __assert_may_alias(c,&e);
  __assert_may_alias(b,&d);
  __assert_no_alias(c,&d);
  __assert_no_alias(b,&e);
}
