/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void foo(int*);
int ss = 20;

int main(){
  int *a, *b, obj, t;
  a = &obj;

  foo(a);

  *a = 200;
  b = &t;

  foo(b);

  __assert_no_alias(a, &ss);
  __assert_no_alias(b, &ss);
}

void foo(int* x) {
  static int* k = &ss;
  printf("%d\n", *k);
  *k = 200;
  k = x;
  *x = 100;
}
