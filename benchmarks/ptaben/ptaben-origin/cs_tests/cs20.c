/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int *obj;
int **x,*b,*w,d;
int **z, *a,*y,c;
void Zulu(int**p, int *q);

void Xray(){
  x = &b;
  w = &d;
  Zulu(x,w);
  __assert_must_alias(b,&d);
  __assert_no_alias(b,&c);
}

void Yank(){
  z = &a;
  y = &c;
  Zulu(z,y);
  __assert_must_alias(a,&c);
  __assert_no_alias(a,&d);
}

void Zulu(int**p,int *q){
  *q = 100;
  *p = q;
}

void main(){
  Xray();
  Yank();
}
