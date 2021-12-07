/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

void foo(int **x, int **y){
  x = y;
}

int main(){
  int *a, *b;
  int a1, b1;
  a = &a1;
  b = &b1;
  foo(a,b);
  __assert_no_alias(a,b);
}
