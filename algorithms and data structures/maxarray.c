#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int maxarray(void *base, size_t nel, size_t width,
             int (*compare)(void *a, void *b)) {
    if (nel == 1)
        return 0;

    size_t index = 0;
    uint8_t *base_ = base;

    for (size_t i = 0; i < nel; ++i)
        if (compare(base_ + i * width, base_ + index * width) > 0)
            index = i;

    return index;
}

int compare(void *a, void *b) { return *(int *)a - *(int *)b; }

int main() {
    int n;
    scanf("%d", &n);
    int arr[n];

    for (int i = 0; i < n; ++i)
        scanf("%d", &arr[i]);

    printf("%d\n", maxarray(arr, n, sizeof(int), compare));

    return 0;
}