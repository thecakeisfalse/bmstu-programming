#include <stdio.h>

typedef long long llong;

int main() {
    llong n, k;
    scanf("%lld", &n);
    llong i, mx, s = 0, A[n];

    for (int i = 0; i < n; ++i)
        scanf("%lld", &A[i]);
    
    scanf("%lld", &k);

    for (i = 0; i < k; ++i)
        s += A[i];

    mx = s;

    for (; i < n; ++i) {
        s += A[i] - A[i-k];
        if (s > mx) mx = s;
    }

    printf("%lld\n", mx);

    return 0;
}