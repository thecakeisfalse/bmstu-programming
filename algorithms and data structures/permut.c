#include <stdio.h>

void main() {
    int a[8] = {0}, i = 0, j = 0, x;
    for (i = 0; i < 8; ++i)
        scanf("%d", &a[i]);
    for (i = 0; i < 8 && j != 8; ++i, a[j] = 1e9)
        for (scanf("%d", &x), j = 0; j < 8 && a[j] != x; ++j)
            ;
    printf("%s\n", (i == 8 && j != 8 ? "YES" : "NO"));
}