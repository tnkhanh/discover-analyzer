#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int inc5(x){
  int y = x + 5;
  return y;
}

int main(int arg) {
  int k = 0;
  while (k < 100) {
    k = inc5(k);
  }
  return k;
}

int main_intra(int arg) {
  int k = 0;
  while (k < 100) {
    k = k + 5;
  }
  return k;
}

