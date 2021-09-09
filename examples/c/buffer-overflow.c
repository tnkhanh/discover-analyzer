#include<stdio.h>
#include<stdlib.h>
#include<assert.h>


int main(int arg) {
    int* x = malloc(2 * sizeof(int));
    *(x+2) = 1;
    return 1;
}
