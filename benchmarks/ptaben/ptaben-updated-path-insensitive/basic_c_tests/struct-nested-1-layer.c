/*
 * Nested structs
 * Author: Sen Ye
 * Date: 06/09/2013
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: June 06, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

struct MyStruct2 {
  int * f3;
  int * f4;
};

struct MyStruct1 {
  int *f1;
  struct MyStruct2 f2;
};

int main()
{
  struct MyStruct1 ms;
  struct MyStruct1 *pms1;
  struct MyStruct2 *pms2;
  int a, b, c;
  ms.f1 = &c;
  ms.f2.f3 = &a;
  ms.f2.f4 = &b;
  pms1 = &ms;
  pms2 = &ms.f2;
  __assert_no_alias(pms2->f4, pms1->f2.f3);
  __assert_must_alias(pms2->f3, pms1->f2.f3);
  return 0;
}
