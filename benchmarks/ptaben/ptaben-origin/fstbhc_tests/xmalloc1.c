/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

void *xmalloc(size_t s) {
  return malloc(s);
}

int main() {
  int *p = xmalloc(sizeof(int));
  int *q = xmalloc(sizeof(int));
  __assert_may_alias(p, q);
  return 0;
}
