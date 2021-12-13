/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 11, 2020
 */

#include "discover.h"
#include <iostream>
#include <vector>

using namespace std;

int global_obj;
int *global_ptr = &global_obj;


int main(int argc, char **argv)
{
  int x;

  vector<int*> vec;
  vec.push_back(&x);

  int *y = vec[0];
  __assert_may_alias(&x, y);

  return 0;
}
