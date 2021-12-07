/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int main(){
  int **a, **b, **c;
  int k;
  a = &b;
  *a = &c;
  *b = &a;
  __assert_must_alias(c, &a);
  __assert_must_alias(b, &c);
  __assert_must_alias(a, &b);
}
