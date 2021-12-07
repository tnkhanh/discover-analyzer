/*
 * Global variables test.
 * Author: Sen Ye
 * Date: 03/05/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: May 31, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>


int *p = NULL;
int *q = NULL;
int c;

void foo() {
  __assert_may_alias(p, q);
}

void bar() {
  q = &c;
}

int main() {
  int a, b;
  p = &a;
  q = p;
  p = &c;
  bar();
  foo();
}
