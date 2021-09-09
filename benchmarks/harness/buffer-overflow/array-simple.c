#include<stdio.h>
#include<stdlib.h>

int main(int arg) {
  char x[10];
  x[11] = 1; // BUG: Buffer Overflow

  int* y = malloc(20 * sizeof(int));
  y[arg] = 2; // BUG: Buffer Overflow

  return 1;
}
