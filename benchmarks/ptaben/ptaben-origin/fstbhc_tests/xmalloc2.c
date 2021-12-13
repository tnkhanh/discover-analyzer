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
  float *q = xmalloc(sizeof(float));
  __assert_no_alias(p, q);
  return 0;
}
