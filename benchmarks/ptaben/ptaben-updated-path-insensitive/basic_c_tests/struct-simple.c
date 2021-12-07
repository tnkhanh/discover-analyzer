/*
 * Alias due to struct assignment
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 06, 2020
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
  int x, y;
  s1.a = &x;
  s2.a = s1.a;
  __assert_must_alias(s2.a, &x);
  return 0;
}
