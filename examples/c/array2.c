#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x = malloc(10 * sizeof(int));
    int* y = x+3;
    *(y+1) = 1;
    return 1;
}
