#include <discover.h>

int main(int a) {
   int x = 0;
   if (a) {
      x = 0;
   } else {
      x = 10;
   }
   __assert_range_full(x, 0, 9);
   __assert_range_full(x, 1, 10);
   __assert_range_full(x, 0, 10);
   __assert_range_lower_bound(x, 0);
   __assert_range_lower_bound(x, 1);
   __assert_range_lower_bound(x, -1);
   __assert_range_upper_bound(x, 10);
   __assert_range_upper_bound(x, 9);
   __assert_range_upper_bound(x, 11);

   return x;    /// Range of x is [0..10]
}

/// Expected output
/// [0..10]
