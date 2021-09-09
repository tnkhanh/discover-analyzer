/*
 * Migrated to Discover: Ta Quang Trung
 * Date: October 28, 2020
 */

#include "discover.h"
#include <deque>

using namespace std;

int global_obj;
int *global_ptr = &global_obj;

int main(int argc, char **argv)
{
  int x;

  deque<int*> adeque;
  adeque.push_back(&x);

  int *y = adeque.front();

  __assert_may_alias(&x, y);

  return 0;
}
