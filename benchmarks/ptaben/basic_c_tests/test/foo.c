#include <stdio.h>
#include <stdlib.h>
#include <discover.h>

int x, y;
int* p;

void foo() {
	p = &y;
}

struct MyStruct {
	void (*fp)();
	int* f1;
};

struct MyStruct context = { foo, &x };

int main()
{
	(*context.fp)();
	int* q = p;
	__assert_must_alias(q, &y);
	return 0;
}
