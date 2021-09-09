#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x;
    *x = *x + 1;  // BUG: there is a NPD here
    return 2;
}
