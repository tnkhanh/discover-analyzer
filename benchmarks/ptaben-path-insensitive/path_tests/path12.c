/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int obj, t,s;
void foo(int**, int**);

void main(){
  int **x, **y;
  int *a, *b, *c, *d,*e;
  e = &t; d = &obj;
  c = &s;
  if(t) { x =&c; y =&e;}
  else { x= &d; y = &d;}

  foo(x,y);

  __assert_no_alias(c,&obj);
  __assert_no_alias(d,&t);
}

void foo(int **p, int **q){
  *p = *q;
}
