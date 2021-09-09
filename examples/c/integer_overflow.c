#include <stdio.h>

int main(void){
    int l;
    int m;
    short s;
    char c;
    l = 0x7fffffff;
    s = l;
    c = l;
    m = l +1;
    printf("l = 0x%x (%d) (%d bits)\n", l, l, sizeof(l) * 8);
    printf("s = 0x%x (%d) (%d bits)\n", s, s, sizeof(s) * 8);
    printf("c = 0x%x (%d) (%d bits)\n", c, c, sizeof(c) * 8);
    printf("m = 0x%x (%d) (%d bits)\n", m, m, sizeof(m) * 8);
    return 0;
}