#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x = malloc(sizeof(int));
    *x = 1;

    int* y = malloc(10 * sizeof(int));
    *(y+3) = 2;

    return 1;
}

