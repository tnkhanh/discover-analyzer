/*
 * Struct with multiple fields.
 * Author: Sen Ye
 * Date: 28/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 06, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct IntChar {
  int f1;
  char f2;
};

struct CharInt {
  char f1;
  int f2;
};

int main() {
  struct IntChar* pint1, *pint2;
  struct IntChar s;
  pint1 = &s;
  pint2 = &s;
  __assert_must_alias(&pint1->f1, &pint2->f1);
  __assert_must_alias(&pint1->f2, &pint2->f2);
  __assert_no_alias(&pint1->f1, &pint2->f2);

  struct CharInt* qint1, *qint2;
  struct CharInt t;
  qint1 = &t;
  qint2 = &t;
  __assert_must_alias(&qint1->f1, &qint2->f1);
  __assert_must_alias(&qint1->f2, &qint2->f2);
  __assert_no_alias(&qint1->f1, &qint2->f2);

  return 0;
}
