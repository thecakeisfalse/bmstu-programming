#include <stdio.h>

typedef long long llong;

int main() {
    llong n, x0, an, f = 0, df = 0;

    scanf("%lld %lld", &n, &x0);

    for (long long i = 0; i <= n; ++i) {
        scanf("%lld", &an);
        f = (f * x0 + an);
        if (i < n)
            df = (df * x0 + an * (n - i));
    }

    printf("%lld %lld\n", f, df);

    return 0;
}