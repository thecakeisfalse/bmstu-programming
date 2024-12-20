#include <stdio.h>
#include <stdlib.h>

typedef long long i64;
typedef long i32;

int i32comp(const void *p1, const void *p2) {
    i32 a = *(i32*)p1,
        b = *(i32*)p2;

    if (a < b)
      return -1;

    return a > b;
}

int main() {
    int n;
    scanf("%d", &n);

    i32 *pref = (i32*)calloc(n+1, sizeof(i32)), x;

    for (int i = 1; i <= n; ++i) {
        scanf("%ld", &x);
        pref[i] = pref[i-1] ^ x;
    }

    qsort(pref, n+1, sizeof(i32), i32comp);

    i64 count = 0;
    for (int l = 0, r = 0; r <= n; ++r) {
        while (pref[l] != pref[r]) ++l;
        count += r-l;
    }

    printf("%lld", count);

    free(pref);
}
