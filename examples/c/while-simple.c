#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int main(int arg) {
  int k = 0;
  int j = 2;
  while (k < 100) {
    k = k + 1;
    j = j + k;
  }
  return k;
}

