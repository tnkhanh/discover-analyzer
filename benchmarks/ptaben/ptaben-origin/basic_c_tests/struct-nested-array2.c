/*
 * Struct with array of structs and pointers.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Sep 20 , 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct InnerArrayStruct {
  int* in1[10];
  char in2;
  double in3;
};

struct MidArrayStruct {
  char mid1;
  struct InnerArrayStruct mid2[5];
  double mid3[20];
};

struct ArrayStruct {
  int out1;
  char out2;
  struct MidArrayStruct out3;
  int* out4;
};

int main() {
  struct ArrayStruct* p;
  struct ArrayStruct s;
  int x, y;

  s.out4 = &x;
  p = &s;
  p->out3.mid2[2].in1[2] = s.out4;
  p->out3.mid2[3].in1[3] = &y;

  __assert_may_alias(p->out3.mid2[1].in1[1], &x);

  __assert_may_alias(s.out3.mid2[3].in1[0], &y);

  return 0;
}
