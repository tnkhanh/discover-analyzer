#include <stdio.h>

int main(void){
    int l, x;
    l = 0x40000000;
    printf("l = %d (0x%x)\n", l, l);
    x = 1 - l;
    printf("l - 0xffffffff = %d (0x%x)\n", x, x);
}