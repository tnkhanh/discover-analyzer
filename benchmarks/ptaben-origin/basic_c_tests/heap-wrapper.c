/*
 * Heap
 * Author: Sen Ye
 * Date: 12/10/2013
 * Description: heap objects are identified according to their
 *    allocation sites.
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 05, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

// return one malloc object
int * my_alloc() {
  int * p = (int *) malloc(sizeof(int));
  return p;
}

int main() {
  int * o1 = my_alloc();
  int * o2 = my_alloc();
  __assert_may_alias(o1, o2);
  return 0;
}
