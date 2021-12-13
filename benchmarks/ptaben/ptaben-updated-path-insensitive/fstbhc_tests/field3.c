/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

// Returns field from function.

struct S {
    int f;
};

int *foo(struct S *s) {
    return &s->f;
}

int main(void) {
    struct S *s = malloc(sizeof(struct S));
    s->f = 300;
    int *i = foo(s);
    __assert_must_alias(&s->f, i);
}
