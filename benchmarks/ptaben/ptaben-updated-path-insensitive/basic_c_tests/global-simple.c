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

int a_int = 10;
int* p_int = &a_int;
int** pp_int = &p_int;

int main() {
  int b_int = a_int;
  int* q_int = p_int;
  int** qq_int = pp_int;
  __assert_must_alias(*qq_int, q_int);
  __assert_must_alias(q_int, &a_int);
  return 0;
}
