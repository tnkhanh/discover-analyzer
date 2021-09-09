/* Test the compilation of global variable in a function call */

#include <stdio.h>
#include <stdlib.h>

int a = 10;
int* b = &a;

void foo(void* a, void* b) {
  return;
}

int main() {
	foo(b, &a);
  int* c = &a;
  foo(b, c);
	return 1;
}
