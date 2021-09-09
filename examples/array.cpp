#include <array>

using namespace std;


class A {
  public:
    virtual void f(int *i) const {
      return;
    }
};

int main(int argc, char **argv)
{
  array<const A *, 2> aarray;
  A *a0 = new A;
  A *a1 = new A;

  aarray[0] = a1;
  aarray[1] = a1;

  return 0;
}
