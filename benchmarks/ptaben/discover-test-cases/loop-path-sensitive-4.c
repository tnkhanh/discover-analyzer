/*
 * Author: Ta Quang Trung
 * Date: Feb 27, 2021
 */

#include "discover.h"
#include "stdlib.h"


int main(int argc, char *argv[]) {
  float *i;
  float *f;

  while (argc-- < 2) {
    void *m = malloc(4);
    i = m;
    f = m;
    __assert_must_alias(i, f);
  }
}
