/*
 * Field cycle.
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
  struct MyStruct * next;
};

int main() {
  struct MyStruct * p = (struct MyStruct *) malloc(sizeof(struct MyStruct));
  int num = 10;
  while (num) {
    p->next = (struct MyStruct *) malloc(sizeof(struct MyStruct));
    p->next->f1 = (int *) malloc(sizeof(int));
    p = p->next;
    num--;
  }
  struct MyStruct *q = p;

  __assert_may_alias(q->next, p->next->next);

  __assert_may_alias(q->f1, p->next->f1);

  return 0;
}
