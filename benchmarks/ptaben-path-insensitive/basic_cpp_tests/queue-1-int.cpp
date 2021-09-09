/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
 */

#include "discover.h"
#include <iostream>
#include <queue>

using namespace std;

int main(int argc, char **argv)
{
  int x;

  queue<int*> aqueue;
  aqueue.push(&x);

  // int *y = aqueue.front();
  // int *y = aqueue.front();
  int *z = aqueue.back();

  // int *z = aqueue[0];
  // aqueue.pop();
  // __assert_may_alias(&x, y);
  // __assert_must_alias(y, z);

  __assert_may_alias(&x, z);

  return 0;
}
