#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int main(int arg) {
  char x[10];
  x[11] = 1; // BUG: Buffer Overflow

  return 1;
}
