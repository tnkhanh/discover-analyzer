/*******************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 *******************************************************************/

/*
 * Builtin library for analyzing C/C++ files
 */

#include <stdio.h>

#ifndef __DEBUG_
#define __DEBUG_ 1                 // 1 to enable, 0 to disable debug
#endif


/*******************************************************************
 * List of bug types
 *******************************************************************/

enum bug_type {
  // Memory bugs
  Memory_leak,
  Memory_null_pointer_deref,
  Memory_buffer_overflow,
  Memory_use_after_free,
  Memory_double_free,
  // Integer bugs
  Integer_overflow,
  Integer_underflow,
  Integer_unsafe_downcast,
  Integer_division_by_zero
};


/*******************************************************************
 * Assertions for alias analysis
 *******************************************************************/

void __assert_no_alias(void* x, void* y) {
#if (__DEBUG_ != 0)
  printf("__assert_no_alias: %p vs. %p: ", x, y);
  if (x != y)
    printf("OK!\n");
  else
    printf("FAILED!\n");
#endif
}

void __refute_no_alias(void* x, void* y) {
#if (__DEBUG_ != 0)
  printf("__refute_no_alias: %p vs. %p: ", x, y);
  if (x == y)
    printf("OK!\n");
  else
    printf("FAILED!\n");
#endif
}

// MUST ALIAS

void __assert_must_alias(void* x, void* y) {
#if (__DEBUG_ != 0)
  printf("__assert_must_alias: %p vs. %p: ", x, y);
  if (x == y)
    printf("OK!\n");
  else
    printf("FAILED!\n");
#endif
}

void __refute_must_alias(void* x, void* y) {
#if (__DEBUG_ != 0)
  printf("__refute_must_alias: %p vs. %p: ", x, y);
  if (x == y)
    printf("FAILED!\n");
  else
    printf("OK!\n");
#endif
}

// MAY ALIAS

void __assert_may_alias(void* x, void* y) {
#if (__DEBUG_ != 0)
  printf("__assert_may_alias: %p vs. %p: ", x, y);
  printf("OK!\n");
#endif
}

void __refute_may_alias(void* x, void* y) {
#if (__DEBUG_ != 0)
  printf("__refute_may_alias: %p vs. %p: ", x, y);
  if (x == y)
    printf("OK!\n");
  else
    printf("FAILED!\n");
#endif
}

/*******************************************************************
 * assertions for memory leak
 *******************************************************************/

void __assert_memory_leak(void* p) {}               // p is leaked

void __assert_some_memory_leak() {}

void __assert_no_memory_leak() {}

/*******************************************************************
 * assertions for range analysis
 *******************************************************************/

// inclusive range: lb <= x < +Inf
void __assert_range_lower_bound(int x, int lb) {
#if (__DEBUG_ != 0)
  printf("__assert_range_lower_bound: %d >= %d", x, lb);
  if (x >= lb)
    printf("OK!\n");
  else
    printf("FAILED!\n");
#endif
}

// inclusive range: -Inf < x <= ub
void __assert_range_upper_bound(int x, int ub) {
#if (__DEBUG_ != 0)
  printf("__assert_range_upper_bound: %d <= %d", x, ub);
  if (x <= ub)
    printf("OK!\n");
  else
    printf("FAILED!\n");
#endif
}

// inclusive range: lb <= x <= ub
void __assert_range_full(int x, int lb, int ub) {
#if (__DEBUG_ != 0)
  printf("__assert_range_full: %d <= %d <= %d", lb, x, ub);
  if ((x >= lb) && (x <= ub))
    printf("OK!\n");
  else
    printf("FAILED!\n");
#endif
}

/*******************************************************************
 * assertions for integer bug
 *******************************************************************/

//void __assert_bug_integer_overflow(int instruction) {}
//void __refute_bug_integer_overflow(int instruction) {}
