#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Date {
    int Day, Month, Year;
};

typedef struct Date Date;

void distributionsort(int min, int max, int index, int n, Date *a) {
    int *count = (int *)calloc(max - min + 1, sizeof(int));
    Date **b = (Date **)calloc(n, sizeof(Date *));

    for (int j = 0; j < n; ++j) {
        int *date = (void *)(&a[j]);
        ++count[date[index] - min];
    }

    for (int i = 1; i < max - min + 1; ++i)
        count[i] += count[i - 1];

    for (int j = n - 1; j >= 0; --j) {
        int *date = (void *)(&a[j]);
        int jndex = (--count[date[index] - min]);
        b[jndex] = (Date *)calloc(1, sizeof(Date));
        memcpy(b[jndex], &a[j], sizeof(Date));
    }

    for (int j = 0; j < n; ++j) {
        a[j] = *b[j];
        free(b[j]);
    }

    free(count);
    free(b);
}

void datesort(int n, Date *a) {
    distributionsort(1, 31, 0, n, a);
    distributionsort(1, 12, 1, n, a);
    distributionsort(1970, 2030, 2, n, a);
}

int main() {
    int n;
    scanf("%d", &n);
    Date a[n];
    for (int i = 0; i < n; ++i)
        scanf("%04d %02d %02d", &a[i].Year, &a[i].Month, &a[i].Day);

    datesort(n, a);

    for (int i = 0; i < n; ++i)
        printf("%04d %02d %02d\n", a[i].Year, a[i].Month, a[i].Day);

    return 0;
}