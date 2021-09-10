/*******************************************************************
 * Author: Ta Quang Trung
 * Date:   July 6th, 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

#include <stdio.h>

/*******************************************************************
 * Original assertions of SVF
 *******************************************************************/

void MUSTALIAS(void* p, void* q){
  printf("\n");
}

void PARTIALALIAS(void* p, void* q){
  printf("\n");
}

void MAYALIAS(void* p, void* q){
  printf("\n");
}

void NOALIAS(void* p, void* q){
  printf("\n");
}

void EXPECTEDFAIL_MAYALIAS(void* p, void* q){
  printf("\n");
}

void EXPECTEDFAIL_NOALIAS(void* p, void* q){
  printf("\n");
}


/*******************************************************************
 * Assertions of Discover
 *******************************************************************/

void __assert_no_alias(void* x, void* y) {
  NOALIAS(x, y);
}

void __refute_no_alias(void* x, void* y) {
  EXPECTEDFAIL_NOALIAS(x, y);
}

void __assert_must_alias(void* x, void* y) {
  MUSTALIAS(x, y);
}

void __refute_must_alias(void* x, void* y) {
  EXPECTEDFAIL_MAYALIAS(x, y);
}

void __assert_may_alias(void* x, void* y) {
  MAYALIAS(x, y);
}

void __refute_may_alias(void* x, void* y) {
  EXPECTEDFAIL_MAYALIAS(x, y);
}
