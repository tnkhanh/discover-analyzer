/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void bar(int**k, int**s){
  *k = *s;
}

void foo(int**x,int**y,int**z){
  int t;
  *y = &t;
  *z = *x;
}

int main(){
  int *p1,*q1,*r1,*a1,*b1,*c1,q2,a2;
  int **p = &p1;
  int **q = &q1;
  q1 = &q2;
  int **r = &r1;
  int **a = &a1;
  a1 = &a2;
  int **b = &b1;
  int **c = &c1;
  bar(&p,&q);
  __assert_must_alias(p,q);
  __assert_no_alias(p,&p1);
  foo(p,q,r);
  __assert_must_alias(q1,r1);
  foo(a,b,c);
  __assert_must_alias(a1,c1);
  __assert_no_alias(q1,c1);
  __assert_no_alias(a1,r1);
}
