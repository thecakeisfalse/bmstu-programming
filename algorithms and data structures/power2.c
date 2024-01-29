#include <stdio.h>

int is_power2(int n) { return n > 0 && (n & (n - 1)) == 0; }

int main() {
    int n;
    scanf("%d", &n);
    int a[n];
    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);

    int summ = 0, count = 0;
    for (int i = 0; i < (1 << n); ++i) {
        summ = 0;
        for (int j = 0; j < n; ++j)
            if (i & (1 << j))
                summ += a[j];

        count += is_power2(summ);
    }

    printf("%d\n", count);

    return 0;
}