/*
 * Simple program to test flow-sensitive analysis.
 * Author: Sen Ye
 * Date: 12/11/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"
#include <stdlib.h>

int * my_malloc(int * q) {
	int *p = malloc(*q);
	return p;
}

int main() {
	int *p, q;
	p = my_malloc(&q);
	return 0;
}
