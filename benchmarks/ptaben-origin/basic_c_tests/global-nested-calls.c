/*
 * Global variable
 * Author: Sen Ye
 * Date: 13/10/2013
 * Description: Initialise global variables in callee and check alias
 *    in caller.
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 06, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int **pp, **qq;
int *p, *q;
int x;

void foo() {
  pp = &p;
  p = &x;
}

void bar() {
  qq = &q;
  q = &x;
}

int main() {
  __assert_may_alias(*pp, *qq);
  foo();
  bar();
  return 0;
}
