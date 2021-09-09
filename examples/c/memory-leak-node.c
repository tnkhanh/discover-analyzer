#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x = malloc(sizeof(int));
    int* y = malloc(sizeof(int));
    *x = 1;
    *y = 2;
    free(x);
    return *y; // BUG: there is a memory leak of y here
}
