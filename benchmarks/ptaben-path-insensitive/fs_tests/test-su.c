/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int **p,**q;
int *x,*y,*z;
int a,b,c;
int main() {
  x=&a;
  y=&b;
  x=&b;
  x=&a;
  z=&b;
  __assert_no_alias(x,y);
  __assert_no_alias(x,z);
  return 0;
}
