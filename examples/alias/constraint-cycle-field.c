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
	struct MyStruct * next;
};

int main() {
	struct MyStruct * p = (struct MyStruct *) malloc (sizeof(struct MyStruct));
	int num = 10;
	while (num) {
		p->next = (struct MyStruct *) malloc (sizeof(struct MyStruct));
		p = p->next;
    num--;
	}
	struct MyStruct *q = p;
  // p->next = q;

	__assert_may_alias(q, p->next);
	// __assert_may_alias(q, p->next->next);
  // __assert_may_alias(q->next, p->next->next);
	return 0;
}
