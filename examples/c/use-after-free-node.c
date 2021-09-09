#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x = malloc(sizeof(int));
    int* y = malloc(sizeof(int));
    *x = 1;
    *y = 2;
    free(x);
    free(y);
    return *x;   // BUG: there is a use-after-free
}
