/*
 * Author: Ta Quang Trung
 * Date: July 17, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main()
{
	int *p, *q;
	int a, b, c;
	if (c) {
		p = &a;
		q = &b;
	}
	else {
		p = &b;
		q = &c;
	}
  // Trung: no alias in path-sensitive analysis
	__assert_no_alias(p,q);
}
