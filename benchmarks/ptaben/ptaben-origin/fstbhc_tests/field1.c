/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

// Same allocation site, same field type
// but different struct type.

struct S {
  int f;
};

struct T {
  int f;
};

void *xmalloc(size_t s) {
  void *v = malloc(s);
  return v;
}

int main(int argc, char *argv[]) {
  struct S *s = xmalloc(sizeof(struct S));
  struct T *t = xmalloc(sizeof(struct T));
  __assert_no_alias(&s->f, &t->f);
}
