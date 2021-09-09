#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

int dec(int x) {
  return x-1;
}

int inc(int x) {
  return x+1;
}

int inc2(int x) {
  return x+1;
}

int foo() {
  int k = 0;  // k=>[0,0]
  while (k < 100) {
    int i = 0;  // i=>[0,0]
    int j = k;  // j=>[0,100)
    while (i < j) {
      i = i + 1;  // i=>[1,99]
      j = j - 1;  // j=>[-1,99)
    }
    k = k + 1; // k=>[1,100]
  }
  return k;  // k=> [100,100]
}

int main() {
  int k = 0;  // k=>[0,0]
  while (k < 100) {
    int i = 0;  // i=>[0,0]
    int j = k;  // j=>[0,100)
    while (i < j) {
      i = inc(i);
      j = dec(j);
    }
    k = inc2(k);
  }
  return k;  // k=> [100,100]
}

