/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int obj,b;

void bar(int **s){
  *s = &b;
}

void foo(int **p){
  *p = &obj;
  bar(p);
}

main(){
  int **x;
  int *a, *c;
  x = &a;
  foo(x);
  __assert_must_alias(a,&b);
  __assert_no_alias(a, &obj);
  x = &c;
  foo(x);
  __assert_must_alias(c,&b);
  __assert_no_alias(c,&obj);

}
