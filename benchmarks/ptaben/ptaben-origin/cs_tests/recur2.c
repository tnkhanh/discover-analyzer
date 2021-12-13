/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int* x,y;
void f(int *m){
  __assert_must_alias(m,&y);
  int *n;
  if(y==1){
    n=&y;
    f(n);
  }
}

int main(){
  x=&y;
  f(x);
}
