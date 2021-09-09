/*
 * Migrated to Discover: Ta Quang Trung
 * Date: June 05, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main(){

  int **a, *b, *x ,c;
  c = 10;
  a = &b;
  b = &c;
  x = *a;
  int y = *x;
  __assert_must_alias(x,&c);
  __assert_must_alias(x,b);
  return 0;
}
