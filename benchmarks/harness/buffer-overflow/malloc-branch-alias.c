#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int foo() {
  return 1;
}

int main(int arg) {
  int *x;

  if (arg > 5) {
    x = malloc(20 * sizeof(int));
    int y = foo();
    x[22] = 2; // BUG: Buffer Overflow
  }
  else {
    x = malloc(40 * sizeof(int));
    x[22] = 1;
    char* u = x;
    u[2] = 1;
    long* v = x;
    v[3] = 2;
  }

  int y = x[22];  //
  free(x);

  return y;
}
