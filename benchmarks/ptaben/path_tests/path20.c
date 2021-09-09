/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int**a,**b, *f,*g,r,w,q,*obj,k;
  f = &k;
  if(a){
    a = &f;
    f = &r;
  }
  else{
    a = &g;
    g = &w;
  }
  a = b;

  *a = &q;
  obj = *b;
  __assert_must_alias(obj,&q);
  __assert_no_alias(obj,&r);
  __assert_no_alias(obj,&w);
}
