#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void strjoin(char *src, char *dest) {
    int ss = strlen(src), sd = strlen(dest), i = 0;
    for (i = 0; i < ss && strncmp(src + i, dest, fmin(ss - i, sd)); ++i)
        ;
    memcpy(src + i, dest, sd);
}

void superstr_rec(char *buffer, int size, int k, char **s) {
    if (k == 0)
        return;

    int old_size = strlen(buffer), mn = 1e9;
    char *best_buffer = (char *)calloc(size + 1, sizeof(char));
    for (int i = 0; k >> i; ++i) {
        if (k & (1 << i)) {
            strjoin(buffer, s[i]);
            superstr_rec(buffer, size, k ^ (1 << i), s);
            if (strlen(buffer) < mn) {
                mn = strlen(buffer);
                memcpy(best_buffer, buffer, size);
            }
            memset(buffer + old_size, 0, size - old_size);
        }
    }

    memcpy(buffer, best_buffer, size);
    free(best_buffer);
}

char *superstr(int n, char **s) {
    int length = 0;
    for (int i = 0; i < n; length += strlen(s[i++]))
        ;
    char *buffer = (char *)calloc(length + 1, sizeof(char));
    superstr_rec(buffer, length, (1LL << n) - 1, s);
    return buffer;
}

int main() {
    int n;
    scanf("%d\n", &n);
    char **s = (char **)calloc(n, sizeof(char *));

    for (int i = 0; i < n; ++i) {
        s[i] = (char *)calloc(1001, sizeof(char));
        scanf("%s", s[i]);
    }

    char *result = superstr(n, s);
    printf("%d\n", strlen(result));

    for (int i = 0; i < n; ++i)
        free(s[i]);
    free(s);
    free(result);

    return 0;
}