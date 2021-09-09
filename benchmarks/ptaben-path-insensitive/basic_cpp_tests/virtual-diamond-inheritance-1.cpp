/*
 * Migrated to Discover: Ta Quang Trung
 * Date: November 06, 2020
 */

#include "discover.h"

/*
 * inheritance relation:
 *
 *       +-------- B <---+
 *       | virtual       |
 * A <---+               +--- D
 *       | virtual       |
 *       +-------- C <---+
 *
 */

int global_obj_f;
int *global_ptr_f = &global_obj_f;

int global_obj_g;
int *global_ptr_g = &global_obj_g;

int global_obj_h;
int *global_ptr_h = &global_obj_h;

int global_obj_l;
int *global_ptr_l = &global_obj_l;

class A {
  public:
  virtual void f(int *i) {
    __assert_must_alias(global_ptr_f, i);
    __assert_no_alias(global_ptr_g, i);
    __assert_no_alias(global_ptr_h, i);
    __assert_no_alias(global_ptr_l, i);
  }
};

class B: public virtual A {
  public:
  virtual void g(int *i) {
    __assert_no_alias(global_ptr_f, i);
    __assert_must_alias(global_ptr_g, i);
    __assert_no_alias(global_ptr_h, i);
    __assert_no_alias(global_ptr_l, i);
  }
};

class C: public virtual A {
  public:
  virtual void h(int *i) {
    __assert_no_alias(global_ptr_f, i);
    __assert_no_alias(global_ptr_g, i);
    __assert_must_alias(global_ptr_h, i);
    __assert_no_alias(global_ptr_l, i);
  }
};

class D: public B, public C {
  public:
  virtual void l(int *i) {
    __assert_no_alias(global_ptr_f, i);
    __assert_no_alias(global_ptr_g, i);
    __assert_no_alias(global_ptr_h, i);
    __assert_must_alias(global_ptr_l, i);
  }
};


int main(int argc, char **argv)
{
  int *ptr_f = &global_obj_f;
  int *ptr_g = &global_obj_g;
  int *ptr_h = &global_obj_h;
  int *ptr_l = &global_obj_l;

  A *pa;
  B *pb;
  C *pc;
  D *pd;

  D d;

  pa = &d;
  pa->f(ptr_f);

  pb = &d;
  pb->g(ptr_g);

  pc = &d;
  pc->h(ptr_h);

  pd = &d;
  pd->l(ptr_l);

  return 0;
}
