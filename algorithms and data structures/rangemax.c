#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int max(int a, int b) { return (a > b ? a : b); }

int min(int a, int b) { return (a < b ? a : b); }

void build(int v[], int i, int a, int b, int *T) {
    assert(T != NULL);

    if (a == b) {
        T[i] = v[a];
    } else {
        int m = (a + b) / 2;
        build(v, i * 2, a, m, T);
        build(v, i * 2 + 1, m + 1, b, T);
        T[i] = max(T[i * 2], T[i * 2 + 1]);
    }
}

int query(int l, int r, int i, int a, int b, int *T) {
    assert(T != NULL);

    if (l == a && r == b)
        return T[i];

    int m = (a + b) / 2;

    if (r <= m)
        return query(l, r, i * 2, a, m, T);
    else if (l > m)
        return query(l, r, i * 2 + 1, m + 1, b, T);

    return max(query(l, m, i * 2, a, m, T),
               query(m + 1, r, i * 2 + 1, m + 1, b, T));
}

void update(int j, int v, int i, int a, int b, int *T) {
    assert(T != NULL);

    if (a == b) {
        T[i] = v;
    } else {
        int m = (a + b) / 2;
        if (j <= m)
            update(j, v, 2 * i, a, m, T);
        else
            update(j, v, 2 * i + 1, m + 1, b, T);

        T[i] = max(T[2 * i], T[2 * i + 1]);
    }
}

int main() {
    int n;
    scanf("%d", &n);
    int a[n];

    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);

    int *T = (int *)calloc(4 * n, sizeof(int));
    build(a, 1, 0, n - 1, T);

    char s[4] = {0};

    while (1) {
        scanf("%s", s);
        if (s[0] == 'E') {
            break;
        } else if (s[0] == 'M') {
            int l, r;
            scanf("%d %d", &l, &r);
            printf("%d\n", query(l, r, 1, 0, n - 1, T));
        } else if (s[0] == 'U') {
            int i, v;
            scanf("%d %d", &i, &v);
            update(i, v, 1, 0, n - 1, T);
        }
    }

    free(T);

    return 0;
}