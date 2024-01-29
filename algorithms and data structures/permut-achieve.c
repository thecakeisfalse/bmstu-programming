#include <stdbool.h>
#include <stdio.h>

bool permuts(int a[], unsigned char k, int i, int b[], int is_equal) {
    if (k == 0)
        return is_equal;

    bool any = false;
    for (int j = 0; j < 8; ++j)
        if (k & (1 << j))
            any =
                permuts(a, k ^ (1 << j), i + 1, b, is_equal && b[i] == a[j]) ||
                any;

    return any;
}

void main() {
    int a[8] = {0}, b[8] = {0}, i = 0;
    for (i = 0; i < 8; ++i)
        scanf("%d", &a[i]);
    for (i = 0; i < 8; ++i)
        scanf("%d", &b[i]);
    printf("%s\n", (permuts(a, 255, 0, b, true) ? "YES" : "NO"));
}
