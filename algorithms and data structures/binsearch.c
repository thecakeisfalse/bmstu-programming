#include <stdio.h>

unsigned long binsearch(unsigned long nel, int (*compare)(unsigned long i)) {
    unsigned long l = 0, r = nel - 1;
    while (l <= r) {
        unsigned long m = (r + l) / 2;
        int res = compare(m);
        if (res == 0) return m;
        else if (res > 0) r = m - 1;
        else if (res < 0) l = m + 1;
    }
    return nel;
}