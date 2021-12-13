/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

void foo(int ***,int**);
int obj;
int main(){

  int ***p, **q, **a, **b, *c, *m,*n,d;
  m = &d;
  a = b = &c;

  if(a){
    p = &a;
    q = &c;
    foo(p,q);
  }
  else{
    p = &b;
    q = &c;
    foo(p,q);
  }

  *a = m;
  n = *b;

  __assert_must_alias(a, b);

  __assert_must_alias(m,n);
}

void foo(int ***x,int **y){
  *x = y;
}
