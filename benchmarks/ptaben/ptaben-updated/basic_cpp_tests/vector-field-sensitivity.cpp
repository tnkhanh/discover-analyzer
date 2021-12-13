/*
 * Simple alias check
 * Author: dye
 * Date: 07/12/2016
 *
 * Migrated to Discover: Ta Quang Trung
 * Date: November 11, 2020
 */

#include "discover.h"
#include <vector>

using namespace std;

class C {
public:
    int f1;
    int f2;
};

vector<C> g;


int main(int argc, char *argv[]) {

    C c_;
    g.push_back(c_);
    C &c = g[0];

    __assert_no_alias(&c_.f1, &c_.f2);
    __assert_no_alias(&c.f1, &c.f2);

    return 0;
}
