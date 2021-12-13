/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int *x, y, z;

void f() {
  if(z>5) return;
  z++;
  if (1) {
    x = &y;
    __assert_must_alias(x,&y);
    f();
    x = &z;
    __assert_must_alias(x,&z);
    __assert_no_alias(x,&y);
    f();
  }
}


int main()
{
  f();
  return 0;
}
