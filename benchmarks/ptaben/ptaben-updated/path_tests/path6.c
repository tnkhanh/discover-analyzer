/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int **p,*q;
  int *a,*b,c,d;

  if(a){
    p = &a;
    q = &c;
  }
  else if(b) {
    p = &b;
    q = &d;
  }
  else{
    printf("dummy branch\n");
  }

  *p = q;
  __assert_may_alias(a,&c);
  __assert_may_alias(b,&d);
  __assert_no_alias(a,&d);
  __assert_no_alias(b,&c);
}
