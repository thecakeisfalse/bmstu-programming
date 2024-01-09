#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void insertsort(int n, int * a) {
    for (int i = 1; i < n; ++i) {
        int elem = a[i], pos = i - 1;
        for (; pos >= 0 && abs(a[pos]) > abs(elem); --pos)
            a[pos+1] = a[pos];
        a[pos+1] = elem;
    }
}

void merge(int k, int l, int m, int * a) {
    int * t = (int *)calloc(m-k+1, sizeof(int)), i, j, h;

    for (i = k, j = l + 1, h  = 0; h < m - k + 1; ++h)
        t[h] = (j <= m && (i == l + 1 || abs(a[j]) < abs(a[i]))) ? a[j++] : a[i++];

    memcpy(a+k, t, h*sizeof(int));
    free(t);
}

void mergesort_rec(int l, int r, int * a) {
    if (r - l + 1 < 5) {
        insertsort(r-l+1, a+l);
    } else if (l < r) {
        int m = (l + r) / 2;
        mergesort_rec(l, m, a);
        mergesort_rec(m+1, r, a);
        merge(l, m, r, a);
    }
}

void mergesort(int n, int * a) {
    mergesort_rec(0, n - 1, a);
}

int main() {
    int n; scanf("%d", &n);
    int a[n];
    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);

    mergesort(n, a);

    for (int i = 0; i < n; ++i)
        printf("%d ", a[i]);
    printf("\n");

    return 0;
}