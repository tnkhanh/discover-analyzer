#include <stdio.h>

int main(int arg) {
  int a = 1;
  int b = a + arg;
  if (b > 1) {
    b = 2;
  }
  else {
    int c = b;
    b = c + arg;
    b = 3;
  }
  return b;
}
