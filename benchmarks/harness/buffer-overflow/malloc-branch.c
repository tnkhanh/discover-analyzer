#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int foo() {
  return 1;
}

int main(int arg) {
  int *x;
  /* char *u; */

  if (arg > 5) {
    x = malloc(20 * sizeof(int));
    int y = foo();
    x[22] = 2; // BUG: Buffer Overflow
  }
  else {
    x = malloc(40 * sizeof(int));
    x[22] = 1;
    char* u = x;
    long* v = u;
    v[1] = 2;
    /* u = x; */
  }

  int y = x[22];  //
  /* u[3]= 1; */
  free(x);

  return y;
}
