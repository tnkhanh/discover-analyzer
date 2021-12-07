/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int main(){
    int ***m,**n,*z,*y,z1,y1;
    m=&n;
    n=&z;
    *m=&y;
    __assert_must_alias(n,&y);
    __assert_no_alias(n,&z);
    z=&z1;
    y=&y1;
    ***m=10;
    z=**m;
    __assert_no_alias(z,&z1);
}
