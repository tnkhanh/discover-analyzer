/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 16, 2020
 */

#include "discover.h"
#include "stdlib.h"

void foo(int**,int* );
int main(){

    int **p,*q;
    int *a,*b,c,d;
    if(a){
        p = &a;
        q = &c;
        foo(p,q);
    }
    else{
        p = &b;
        q = &d;
        foo(p,q);
    }

    *p = q;
    __assert_may_alias(a,&c);
    __assert_may_alias(b,&d);
    __assert_no_alias(a,&d);
    __assert_no_alias(b,&c);
}

void foo(int**x,int *y){
    *x = y;
}
