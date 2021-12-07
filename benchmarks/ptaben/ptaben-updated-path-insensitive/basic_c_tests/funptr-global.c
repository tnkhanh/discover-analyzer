/*
 * functionpointer1.c
 * Test function pointer with field initialization of globals
 *
 *  Created on: 01/09/2015
 *      Author: Yulei Sui
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: August 12, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

typedef int PRSize;
typedef unsigned int PRUint32;
typedef unsigned int PRUintn;
typedef int PRIntn;

struct PLHashAllocOps {
  void *(*allocTable)(void *pool , PRSize size ) ;
};

typedef struct PLHashAllocOps PLHashAllocOps;

static void *DefaultAllocTable(void *pool , PRSize size ) {
  void *tmp ;
  {
    tmp = malloc((unsigned int )size);
    return (tmp);
  }
}

PLHashAllocOps defaultHashAllocOps  = {& DefaultAllocTable};

void PL_NewHashTable(PRUint32 n , void *allocPriv ) {
  void *tmp___0 ;
  void *tmp___1 ;

  PLHashAllocOps const   * allocOps = (PLHashAllocOps const   *)(& defaultHashAllocOps);
  tmp___0 = (*(allocOps->allocTable))(allocPriv, (int )sizeof(int));
  tmp___1 = (*(allocOps->allocTable))(allocPriv, (int )sizeof(int));
  // TRUNG: originally MAYALIAS in PTABench.
  // _no_alias since they are from different malloc operator.
  __assert_no_alias(tmp___0,tmp___1);

}

int main() {
  PRUint32 n;
  void *allocPriv = NULL;
  PL_NewHashTable(n, allocPriv);
  return 1;
}
