#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
    int n, m, x;
    scanf("%d %d", &n, &m);
    int rows[n], columns[m];

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            scanf("%d", &x);
            columns[j] = (i == 0 ? x : fmin(columns[j], x));
            rows[i] = (j == 0 ? x : fmax(rows[i], x));
        }
    }

    int max = 0, min = 0;

    for (int i = 0; i < n; ++i)
        if (rows[i] < rows[max])
            max = i;

    for (int i = 0; i < m; ++i)
        if (columns[i] > columns[min])
            min = i;

    if (columns[min] == rows[max])
        printf("%d %d\n", max, min);
    else
        printf("none\n");

    return 0;
}