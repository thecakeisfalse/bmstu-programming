#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int gcd(int a, int b) {

    while (a > 0 && b > 0) {
        if (a > b)
            a %= b;
        else
            b %= a;
    }
    return a + b;
}

void build(int v[], int i, int a, int b, int *T) {
    assert(T != NULL);

    if (a == b) {
        T[i] = fabs(v[a]);
    } else {
        int m = (a + b) / 2;
        build(v, i * 2, a, m, T);
        build(v, i * 2 + 1, m + 1, b, T);
        T[i] = gcd(T[2 * i], T[2 * i + 1]);
    }
}

int query(int l, int r, int i, int a, int b, int *T) {
    assert(T != NULL);

    if (l == a && b == r)
        return T[i];

    int m = (a + b) / 2;

    if (r <= m)
        return query(l, r, i * 2, a, m, T);

    else if (l > m)
        return query(l, r, i * 2 + 1, m + 1, b, T);

    return gcd(query(l, m, i * 2, a, m, T),
               query(m + 1, r, i * 2 + 1, m + 1, b, T));
}

int main() {
    int n;
    scanf("%d", &n);
    int a[n];
    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);
    int *T = (int *)calloc(4 * n, sizeof(int));

    build(a, 1, 0, n - 1, T);

    int m;
    scanf("%d", &m);
    while (m-- > 0) {
        int l, r;
        scanf("%d %d", &l, &r);
        printf("%d\n", query(l, r, 1, 0, n - 1, T));
    }

    free(T);

    return 0;
}