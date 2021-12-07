/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

void foo(int** s);
void bar(int** s);
int k;
int main(){

  int **p,*q;
  int *b,*c,e;

  if(e) {
    p = &b;
    foo(&q);
  }
  else{
    p = &c;
    q = &e;
  }

  *p = q;
  __assert_may_alias(b, &k);
  __assert_may_alias(c, &e);
  __assert_no_alias(b, &e);
  __assert_no_alias(c, &k);
}

void foo(int**x){
  bar(x);
}

void bar(int**s){
  *s = &k;
}
