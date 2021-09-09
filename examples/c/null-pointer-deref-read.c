#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x;
    int y = *x;  // BUG: there is a NPD here
    return 2;
}
