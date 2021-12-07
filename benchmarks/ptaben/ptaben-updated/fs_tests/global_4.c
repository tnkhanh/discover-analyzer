/*
 * Migrated to Discover: Ta Quang Trung
 * Date: Nov 08, 2020
 */

#include "discover.h"

int g;
int* obj = &g;
void Zulu(int**p, int *q);

void Xray(){

	int **x,*b,*w,d;
	x = &b;
	w = &d;
	Zulu(x,w);
	__assert_no_alias(b,w);
	// __assert_must_alias(b,&g);
}

void Zulu(int**p,int *q){
	*p = q;
	*p = obj;
}

int main(){
	Xray();
	return 0;
}
