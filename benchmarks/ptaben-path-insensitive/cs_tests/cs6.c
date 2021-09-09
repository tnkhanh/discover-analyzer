/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int** g;

void foo(int **p, int *q){
  g = q;
  *p = g;
}

int main(){
  int **a,*b,*a1,b1;
  a = &a1;
  b = &b1;
  foo(a,b);
  __assert_must_alias(a1,b);
}
