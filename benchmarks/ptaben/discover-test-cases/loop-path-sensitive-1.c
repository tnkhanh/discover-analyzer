/*
 * Author: Ta Quang Trung
 * Date: Feb 27, 2021
 */

#include "discover.h"
#include "stdlib.h"


int main(int argc, char *argv[]) {
  int *i;
  int *f;

  while (argc-- < 2) {
    void *m = malloc(4);
    if (argc) {
      i = m;
    } else {
      f = m;
    }
  }

  __assert_no_alias(i, f);
}
