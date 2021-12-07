/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int foo(int*);

int bar(int *q){
  *q = 100;
  foo(q);
}

int foo(int *a){
  *a = 10;
  if(*a!=100)
    bar(a);
}

int main(){
  int* a,b,c;
  b=0;
  a = &b;
  foo(a);
  a = &c;
  foo(a);
}
