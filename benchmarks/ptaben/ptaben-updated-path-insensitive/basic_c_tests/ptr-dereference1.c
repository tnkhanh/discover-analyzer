/*
 * Simple alias check
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 05, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int main()
{
  int a,b,*c,*d;
  c = &a;
  d = &a;
  __assert_must_alias(c,d);
  c = &b;

  __assert_no_alias(c,d);
  __assert_no_alias(&b,d);
  return 0;
}
