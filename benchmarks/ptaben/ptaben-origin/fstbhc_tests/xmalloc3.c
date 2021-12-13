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
  int *r = xmalloc(sizeof(int));

  __assert_no_alias(p, q);
  __assert_may_alias(p, r);
  __assert_no_alias(q, r);

  return 0;
}
