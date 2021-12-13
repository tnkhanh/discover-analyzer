/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

typedef struct {
  int x;
  void (*myfunction)(float *f, long l);
} str;

float globalf = 3.33333;

void foo(float *f, long l) {
  __assert_no_alias(f, &globalf);
}

void bar(float *f, long l) {
  __assert_must_alias(f, &globalf);
}

str *spec;

void decode(str *hi) {
  hi->myfunction = bar;
}

int main(int argc, char *argv[]) {
  spec = malloc(5000);
  decode(&spec[argc]);
  (*spec[argc].myfunction)(&globalf, 555);

  float localf;
  foo(&localf, 0);
}
