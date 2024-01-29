#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swap(int *a, int *b) {
    int t = *a;
    *a = *b;
    *b = t;
}

void selectsort(int n, int *a) {
    for (int j = n - 1, k, i; j > 0; --j) {
        for (k = j, i = j - 1; i >= 0; --i)
            if (a[k] < a[i])
                k = i;
        swap(&a[j], &a[k]);
    }
}

int partition(int l, int r, int *a) {
    int pivot = a[r], q = l;

    for (int i = l; i < r; ++i)
        if (a[i] < pivot)
            swap(&a[i], &a[q++]);

    swap(&a[q], &a[r]);

    return q;
}

void quicksort_rec(int l, int r, int *a, int m) {
    if (r - l + 1 < m) {
        selectsort(r - l + 1, a + l);
    } else if (l < r) {
        int q = partition(l, r, a);
        quicksort_rec(l, q - 1, a, m);
        quicksort_rec(q + 1, r, a, m);
    }
}

void quicksort(int n, int *a, int m) { quicksort_rec(0, n - 1, a, m); }

int main() {
    int n, m;
    scanf("%d %d", &n, &m);
    int a[n];
    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);

    quicksort(n, a, m);

    for (int i = 0; i < n; ++i)
        printf("%d ", a[i]);
    printf("\n");

    return 0;
}