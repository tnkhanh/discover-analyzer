#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int main(int arg) {
  int x = 1;
  int y = 2;
  while (x>y) {
    x = 3;
    y = y + 1;
  }
  return x + y;
}
