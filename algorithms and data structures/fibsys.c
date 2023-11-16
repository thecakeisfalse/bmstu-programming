#include <stdio.h>

typedef unsigned long long ullong;

int main() {
    ullong x, i = 2, a[100] = {0, 1};
    scanf("%llu", &x);

    for (i = 2; (a[i] = a[i - 1] + a[i - 2]) <= x; i++)
        ;

    if (x == 0)
        printf("0");

    while (i > 2) {
        printf("%d", a[--i] <= x);
        x -= a[i] * (a[i] <= x);
    }

    return 0;
}
