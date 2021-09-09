/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int **x, *y;
int z;
void f(int **p);
void main(){
  x = &y;
  f(x);
}


void f(int **p){
  int k;
  y = &k;
  if (z){
    f(p);
    *p = &z;

    __assert_must_alias(y, &z);
    __assert_no_alias(y, &k);

    f(p);
  }
  /// y will not alias to &z as the value flow
  /// of y after it is updated at "*p=&z" will
  /// flow into f(p) again and then be updated
  /// by the first statement "y=&k".

  __assert_no_alias(y, &z);
  __assert_may_alias(y, &k);

}
