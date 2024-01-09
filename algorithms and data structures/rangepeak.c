#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int peak(int i, int n, int a[]) {
    return (0 <= i && i < n) && (i > 0 ? a[i-1] <= a[i] : 1) && (i < n - 1 ? a[i+1] <= a[i] : 1);
}

int query_i(int i, int * T) {
    int v = 0;

    for (; i >= 0; i = (i & (i + 1)) - 1)
        v += T[i];

    return v;
}

int query(int l, int r, int * T) {
    return query_i(r, T) - query_i(l-1, T);
}

void update(int i, int delta, int n, int * T) {
    for (; 0 <= i && i < n; i |= (i + 1))
        T[i] += delta;
}

int main() {
    int n; scanf("%d", &n);
    int a[n];
    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);

    int * T = (int *)calloc(n, sizeof(int));

    for (int i = 0; i < n; ++i)
        update(i, peak(i, n, a), n, T);

    char s[4] = {0};

    while (1) {
        scanf("%s", s);
 
        if (s[0] == 'E') {
            break;
        } else if (s[0] == 'P') {
            int l, r; scanf("%d %d", &l, &r);
            printf("%d\n", query(l, r, T));
        } else if (s[0] == 'U') {
            int i, v; scanf("%d %d", &i, &v);
            int z1 = peak(i-1, n, a), z2 = peak(i, n, a), z3 = peak(i+1, n, a);
            a[i] = v;
            update(i-1, peak(i-1, n, a) - z1, n, T);
            update(i, peak(i, n, a) - z2, n, T);
            update(i+1, peak(i+1, n, a) - z3, n, T);
        }
    }

    free(T);

    return 0;
}