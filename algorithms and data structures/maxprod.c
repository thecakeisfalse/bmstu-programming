#include <assert.h>
#include <math.h>
#include <stdio.h>

typedef long double logfrac;

int main() {
    assert(sizeof(int) == 4);

    int n;
    scanf("%d", &n);
    logfrac v[n];

    for (int i = 0; i < n; ++i) {
        int a, b;
        scanf("%d/%d", &a, &b);
        v[i] = log2(a) - log2(b);
    }

    logfrac mx = v[0], s = 0;
    int ml = 0, mr = 0, p = 0;

    for (int i = 0; i < n; ++i) {
        s += v[i];
        if (v[i] > s) {
            s = v[i];
            p = i;
        }

        if (s > mx) {
            mx = s;
            ml = p;
            mr = i;
        }
    }

    printf("%d %d\n", ml, mr);

    return 0;
}