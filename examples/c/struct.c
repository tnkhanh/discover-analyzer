#include<stdio.h>
#include<stdlib.h>

struct foo {
  int x;
  int y;
};

int main(int argc, char **argv) {
  struct foo a;
  a.x = 59;

  struct foo* c = malloc(sizeof(struct foo));
  c->x = 1;

  return 2;
}
