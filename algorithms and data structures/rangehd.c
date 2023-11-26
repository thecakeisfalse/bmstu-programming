#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define N 1000001

int query_i(int i, int * T) {
    int v = 0;

    for (; i >= 0; i = (i & (i + 1)) - 1)
        v ^= T[i];

    return v;
}

int query(int l, int r, int * T) {
    return query_i(r, T) ^ query_i(l-1, T);
}

void update(int i, int delta, int n, int * T) {
    for (; i < n; i |= (i + 1))
        T[i] ^= delta;
}

int main() {
    char a[N]; scanf("%s", a);
    int n = strlen(a);
    int * T = (int *)calloc(n, sizeof(int));

    for (int i = 0; i < n; ++i)
        update(i, (1 << (a[i] - 'a')), n, T);

    char s[4] = {0};

    while (1) {
        scanf("%s", s);
 
        if (s[0] == 'E') {
            break;
        } else if (s[0] == 'H') {
            int l, r;
            scanf("%d %d", &l, &r);
            int result = query(l, r, T);
            printf("%s\n", (!(result & (result - 1)) ? "YES" : "NO"));
        } else if (s[0] == 'U') {
            int i; char v[N];
            scanf("%d %s", &i, v);
            for (int j = 0; v[j] != 0; ++j) {
                int delta = (1 << (a[i + j] - 'a')) ^ (1 << (v[j] - 'a'));
                update(i + j, delta, n, T);
                a[i + j] = v[j];
            }
        }
    }

    free(T);

    return 0;
}