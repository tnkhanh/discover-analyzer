/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main(){
  int **p,**q;
  int **x,**y;
  int *a,*b,*x1,a1,b1;
  int *m,*n,n1;
  /// Note that n needs to be initialized
  n = &n1;
  a = &a1;
  b = &b1;
  x = y = &x1;
  p = q = &a;
  if(a){
    p = x;
    q = y;
  }

  *p = n;
  m = *q;

  __assert_may_alias(p,&a);
  __assert_may_alias(p,&x1);
  __assert_no_alias(m,&a1);
  __assert_no_alias(m,&x1);
  __assert_must_alias(n,m);
}
