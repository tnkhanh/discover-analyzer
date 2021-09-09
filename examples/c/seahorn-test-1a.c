// RUN: %sea abc -O0 --abc-encoding=%abc_encoding %dsa "%s" %abc3_definitions 2>&1 | OutputCheck %s
// CHECK: ^unsat$

// See how we treat global variables, struct and one-dimensional arrays
// The program is safe in terms of buffer overflow although A is
// uninitialized. B is initialized since it is a global array.
struct foo {
  int x;
  int y;
};

int main(int argc, char **argv) {
  struct foo a;
  a.x = 59;
}
