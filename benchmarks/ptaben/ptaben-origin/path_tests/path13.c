/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

struct agg{
  int **i;
}agg;

int main(){
  int *b,*c,*d,f,w;
  struct agg ag1, *a;
  a = &ag1;

  if(a){
    if(f){
      a->i = &c;
      b = &f;
    }
  }
  else{
    a->i = &d;
    b = &w;
  }

  *(a->i) = b;
  __assert_may_alias(c, &f);
  __assert_may_alias(d, &w);
  __assert_no_alias(c, &w);
  __assert_no_alias(d, &f);
}
