/*
 * Author: Ta Quang Trung
 * Date: September 29, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct MyStruct {
  struct MyStruct * next;
};

int main() {
  struct MyStruct * p = (struct MyStruct *) malloc (sizeof(struct MyStruct));
  p->next = p;

  __assert_must_alias(p, p->next);
  return 0;
}
