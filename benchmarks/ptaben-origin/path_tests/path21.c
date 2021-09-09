/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int ***p,**a,**b,*q,*r,*f,v,z,*g,f1;

  p = &a;
  a = b = &f;
  f = &f1;
  q = &v;

  if(a){
    f = &z;
    *p = &g;
    b = *p;
  }
  else{

  }

  *a = q;
  r = *b;
  __assert_must_alias(r,q);
  __assert_no_alias(r,&z);
  __assert_no_alias(r,&f1);
}
