#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int main(int arg) {
  char x[10];
  x[11] = 1; // BUG: Buffer Overflow

  /* int* y = malloc(20 * sizeof(int)); */
  /* y[22] = 2; // BUG: Buffer Overflow */
  /* y[33] = 3; // BUG: Buffer Overflow */

  /* int* z = y+1; */
  /* z[18] = 4;      // OK */
  /* z[19] = 5;      // BUG: Buffer Overflow */

  int* u = x;
  /* u[12] = 6; */

  char* v = u;
  v[9] = 'a';     // No BUG Buffer Overflow here
  x[9] = 'b';

  /*
    Observation: 
  */

  /* int* t = z+3; */
  /* t[16] = 8; */
  /* t[17] = 9; */

  return 1;
}
