#include <stdio.h>

int is_power2(int n) { return n > 0 && (n & (n - 1)) == 0; }

int combinations(int arr[], int n, int sum, int j) {
    int count = is_power2(sum);
    for (int i = j; i < n; i++)
        count += combinations(arr, n, sum + arr[i], i + 1);

    return count;
}

int main(void) {
    int n;
    scanf("%d", &n);

    int arr[n];
    for (int i = 0; i < n; i++)
        scanf("%d", &arr[i]);

    printf("%d\n", combinations(arr, n, 0, 0));
    return 0;
}