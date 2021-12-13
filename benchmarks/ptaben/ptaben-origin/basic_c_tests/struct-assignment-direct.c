/*
 * Alias due to struct assignment
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 29, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct s{
  int *a;
  int b;
};

int main()
{
  struct s s1, s2;
  struct s * p1;
  int x, y;
  s1.a = &x;
  s1.b = y;
  s2 = s1;
  __assert_must_alias(s2.a, s1.a);
  return 0;
}
