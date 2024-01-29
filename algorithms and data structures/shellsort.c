#include <stdio.h>

void shellsort(unsigned long nel,
               int (*compare)(unsigned long i, unsigned long j),
               void (*swap)(unsigned long i, unsigned long j)) {
    unsigned long long fib[100] = {0, 1}, i = 2, j, k;

    do
        fib[i] = fib[i - 1] + fib[i - 2];
    while (fib[i] < nel && (++i));

    for (; i > 0; --i)
        for (k = fib[i]; k < nel; ++k)
            for (j = k; j >= fib[i] && compare(j - fib[i], j) > 0; j -= fib[i])
                swap(j, j - fib[i]);
}
