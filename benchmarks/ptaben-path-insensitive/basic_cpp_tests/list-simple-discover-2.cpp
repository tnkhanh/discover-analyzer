/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 1, 2020
 */

#include "discover.h"
#include <list>

using namespace std;

int global_obj1;
int *global_ptr1 = &global_obj1;

// int global_obj2;
// int *global_ptr2 = &global_obj2;

typedef struct node {
  int *data;
} node;

int main(int argc, char **argv)
{
  int *ptr1 = &global_obj1;
  // int *ptr2 = &global_obj2;

  node a = {ptr1};

  list<node> alist;
  alist.push_back(a);

  list<node>::const_iterator it = alist.begin();
  node b = *it;

  __assert_must_alias(b.data, ptr1);

  return 0;
}
