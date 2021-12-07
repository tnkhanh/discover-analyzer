/*
 * Alias due to struct.
 * Author: Sen Ye
 * Date: 10/10/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 29, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct MyStruct {
  int * f1;
  struct MyStruct *f2;
};

int main() {
  struct MyStruct *p, *q;
  struct MyStruct ms1, ms2;
  int x;
  p = &ms1;
  q = &ms1;
  ms1.f2 = &ms2;
  p->f2->f1 = &x;
  // TRUNG: was MAYALIAS in the original PTABen
  __assert_must_alias(q->f2->f1, &x);
  return 0;
}
