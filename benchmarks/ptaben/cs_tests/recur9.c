/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"
#include "stdlib.h"

int z;

void foo(int **a);

void bar(int **q){
  int** a = malloc(10);
  foo(a);
}

void foo(int **a){
  if(z>5) return;
  z++;
  *a = &z;
  bar(a);
}

int main(){
  int** a, *b, *c, b1, c1;
  b = &b1;
  a = &b;
  foo(a);
  __assert_may_alias(b, &z);
  __assert_no_alias(b, &b1);
  a = &c;
  foo(a);
  __assert_may_alias(c, &z);
  __assert_no_alias(c, &c1);
}
