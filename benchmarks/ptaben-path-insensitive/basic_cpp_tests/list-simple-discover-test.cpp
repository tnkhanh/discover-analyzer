#include "discover.h"
#include <list>

using namespace std;

int main(int argc, char **argv)
{
  int x = 1;

  list<int*> alist;
  alist.push_back(&x);

  list<int*>::const_iterator it = alist.begin();
  int* a = *it;

  __assert_must_alias(a, &x);

  return 0;
}
