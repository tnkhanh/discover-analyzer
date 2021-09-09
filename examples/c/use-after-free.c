#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x;
    int* y;
    x = malloc(sizeof(int));
    y = malloc(sizeof(int));
    free(x);
    free(y);
    *x=1;
    return 2;
}
