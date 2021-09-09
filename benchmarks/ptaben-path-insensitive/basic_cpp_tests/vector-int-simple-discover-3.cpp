/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 11, 2020
 */

#include "discover.h"
#include <iostream>
#include <vector>

using namespace std;

int main(int argc, char **argv)
{
  vector<int*> vec;
  // int a;
  // vec.push_back(&a);

  // vector<int*>::const_iterator it = vec.begin();
  // int *b = *it;

  int *c = vec.front();

  int *d = vec[0];

  // __assert_must_alias(b, c);
  __assert_must_alias(c, d);
  // __assert_must_alias(&a, b);
  // __assert_may_alias(&a, b);
  return 0;
}
