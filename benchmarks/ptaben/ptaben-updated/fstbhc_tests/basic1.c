/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

int main() {
  int *p = malloc(sizeof(int));
  int *q = malloc(sizeof(int));
  __assert_no_alias(p, q);
  return 0;
}
