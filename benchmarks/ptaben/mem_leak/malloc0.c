/*
 * Never free leak
 * Author: Yule Sui
 * Date: 02/04/2014
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: May 13, 2020
 */

#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

void f(int*a, int*b);
int main(){
    int *a = malloc(sizeof(int));
    int *b = malloc(sizeof(int));
    int *k  = a;
    *b = 200;
    *a =100;
    *a = 100;
    f(a,b);
    __discover_assert_memory_leak(a);
    __discover_assert_memory_leak(b);
}

void f(int*a, int*b){
    *a = 300;
    *b = 200;
}
