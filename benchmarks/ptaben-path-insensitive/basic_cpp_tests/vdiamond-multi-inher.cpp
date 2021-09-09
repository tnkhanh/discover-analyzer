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
 * A <---+               |
 *       | virtual       |
 *       +-------- C <---+--- E
 *                       |
 *                 D <---+
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

int global_obj_m;
int *global_ptr_m = &global_obj_m;

class A {
  public:
    virtual void f(int *i) {
      __assert_must_alias(global_ptr_f, i);
      __assert_no_alias(global_ptr_g, i);
      __assert_no_alias(global_ptr_h, i);
      __assert_no_alias(global_ptr_l, i);
      __assert_no_alias(global_ptr_m, i);
    }
};

class B: public virtual A {
  public:
    virtual void g(int *i) {
      __assert_no_alias(global_ptr_f, i);
      __assert_must_alias(global_ptr_g, i);
      __assert_no_alias(global_ptr_h, i);
      __assert_no_alias(global_ptr_l, i);
      __assert_no_alias(global_ptr_m, i);
    }
};

class C: public virtual A {
  public:
    virtual void h(int *i) {
      __assert_no_alias(global_ptr_f, i);
      __assert_no_alias(global_ptr_g, i);
      __assert_must_alias(global_ptr_h, i);
      __assert_no_alias(global_ptr_l, i);
      __assert_no_alias(global_ptr_m, i);
    }
};

class D {
  public:
    virtual void l(int *i) {
      __assert_no_alias(global_ptr_f, i);
      __assert_no_alias(global_ptr_g, i);
      __assert_no_alias(global_ptr_h, i);
      __assert_must_alias(global_ptr_l, i);
      __assert_no_alias(global_ptr_m, i);
    }
};

class E: public B, public C, public D {
  public:
    virtual void m(int *i) {
      __assert_no_alias(global_ptr_f, i);
      __assert_no_alias(global_ptr_g, i);
      __assert_no_alias(global_ptr_h, i);
      __assert_no_alias(global_ptr_l, i);
      __assert_must_alias(global_ptr_m, i);
    }
};

int main(int argc, char **argv)
{
  int *ptr_f = &global_obj_f;
  int *ptr_g = &global_obj_g;
  int *ptr_h = &global_obj_h;
  int *ptr_l = &global_obj_l;
  int *ptr_m = &global_obj_m;

  A *pa;
  B *pb;
  C *pc;
  D *pd;
  E *pe;

  E e;

  pa = &e;
  pa->f(ptr_f);

  pb = &e;
  pb->g(ptr_g);

  pc = &e;
  pc->h(ptr_h);

  pd = &e;
  pd->l(ptr_l);

  pe = &e;
  pe->m(ptr_m);

  return 0;
}
