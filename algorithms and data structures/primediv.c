#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SET(a, x) (a |= (1 << (7 - x)))
#define UNSET(a, x) (a &= ~(1 << (7 - x)))
#define BIT(a, x) (a & (1 << (7 - x)))

typedef long long llong;
typedef unsigned char uchar;

int main() {
    llong n;
    scanf("%lld", &n);
    n = llabs(n);

    llong sq = sqrt(n + 1), size = (sq + 7LL) >> 3, last = 1;
    uchar *a = (char *)calloc(size + 1, sizeof(uchar));

    memset(a, 255, size);

    UNSET(a[0], 0);
    UNSET(a[0], 1);

    for (llong d = 2; d * d <= n; ++d)
        if (BIT(a[d >> 3], d & 7))
            for (llong j = d * d; j * j <= n; j += d)
                UNSET(a[j >> 3], j & 7);

    for (llong d = 2; d * d <= n; ++d) {
        while (BIT(a[d >> 3], d & 7) && n % d == 0) {
            n /= d;
            last = d;
        }
    }

    printf("%lld\n", (n == 1 ? last : n));

    free(a);

    return 0;
}
