/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

typedef struct subagg1{
  int *d;
} subagg1;

typedef struct agg1{
  int *c;
  subagg1 sub;
} agg1;

int main(){
  int *a,*b,k1,k2;
  agg1 g1,g2;
  agg1 *g = &g1;

  if(k1){
    g = &g2;
    a = &k1;
  }
  else{
    a = &k2;
  }

  g->sub.d = a;
  __assert_may_alias(g1.sub.d,&k2);
  __assert_may_alias(g2.sub.d,&k1);
  __assert_no_alias(g1.sub.d,&k1);
  __assert_no_alias(g2.sub.d,&k2);

}
