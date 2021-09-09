/*
 * Context-insensitive.
 * Author: Sen Ye
 * Date: 10/10/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: May 24, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int global;
int *p_global;

void foo() {
  p_global = &global;
}

int main() {
  int *p_local;
  p_local = &global;
  foo();
  // Trung: in the original PTABen, the following was MAYALIAS
  __assert_must_alias(p_local, p_global);
  return 0;
}
