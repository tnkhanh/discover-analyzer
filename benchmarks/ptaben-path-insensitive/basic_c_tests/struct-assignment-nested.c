/*
 * Struct assignment.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 29, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct InnerArrayStruct {
  int* in1[10];
  int* in2[20];
  char in3;
};

struct MidArrayStruct {
  char mid1[10];
  struct InnerArrayStruct mid2[5];
};

struct ArrayStruct {
  char out2;
  struct MidArrayStruct out3;
  int* out4;
};

int main() {
  struct ArrayStruct* p;
  struct ArrayStruct s1, s2;
  int x, y;

  s1.out4 = &x;
  p = &s1;
  p->out3.mid2[3].in1[3] = &y;

  s2 = s1;

  __assert_must_alias(s2.out4, &x);

  // Trung: originally MAYALIAS in PTABen
  __assert_no_alias(s2.out3.mid2[1].in1[1], &y);

  // Trung: originally MAYALIAS in PTABen
  __assert_no_alias(s2.out3.mid2[3].in1[20], &y);

  return 0;
}
