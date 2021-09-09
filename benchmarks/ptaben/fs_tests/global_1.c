/*
 * Global pointer in flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 08/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int x, y; int *p = &x; int *q = &y;
int **pp = &p; int**qq = &q;

void foo() {
  __assert_no_alias(*pp, *qq);
}

void bar() {
  qq = &q;
  q = &x;
}

int main() {
  foo();
  bar();
  __assert_must_alias(*pp, *qq);
}
