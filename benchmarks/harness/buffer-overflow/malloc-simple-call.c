#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

void foo(int* y) {
  int* z = y+1;
  z[18] = 4;      // OK
  z[19] = 5;      // BUG: Buffer Overflow
}

void bar(int* y) {
  int* z = y;
  z[18] = 4;      // OK
  z[19] = 5;      // BUG: Buffer Overflow
}

int main(int arg) {
  char x[10];
  x[11] = 1; // BUG: Buffer Overflow

  int* y = malloc(20 * sizeof(int));
  y[22] = 2; // BUG: Buffer Overflow
  y[33] = 3; // BUG: Buffer Overflow

  // note: Infer can detect only 1 BOF bug, not all 3
  foo(y);

  foo(y+1);

  bar(y+1);

  return 1;
}
