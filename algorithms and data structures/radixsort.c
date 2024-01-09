#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef union {
    int x;
    unsigned char bytes[4];
} Int32;

void radixsort(int n, Int32 * a) {
    int count[1 << 8] = {0};
    Int32 * b = (Int32 *)calloc(n, sizeof(Int32));

    for (int i = 0; i < 4; ++i) {
        memset(count, 0, sizeof(count));

        for (int j = 0; j < n; ++j)
            ++count[(unsigned char)(a[j].bytes[i] - ((i == 3) << 7))];

        for (int j = 1; j < (1 << 8); ++j)
            count[j] += count[j-1];

        for (int j = n - 1; j >= 0; --j)
            b[--count[(unsigned char)(a[j].bytes[i] - ((i == 3) << 7))]] = a[j];

        for (int j = 0; j < n; ++j)
            a[j] = b[j];
    }

    free(b);
}

int main() {
    int n; scanf("%d", &n);
    Int32 a[n];
    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);

    radixsort(n, a);

    for (int i = 0; i < n; ++i)
        printf("%d ", a[i]);
    printf("\n");

    return 0;
}
