#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int * prefix(char * s, size_t n) {
    int * p = (int *)calloc(n, sizeof(int));

    for (int t = 0, i = 1; i < n; p[i++] = t) {
        while (t > 0 && s[t] != s[i])
            t = p[t - 1];

        t += (s[t] == s[i]);
    }

    return p;
}

int * kmpall(char * s, char * t) {
    size_t length_s = strlen(s),
           length_t = strlen(t),
           size = 10, m = 0;
    int *p = prefix(s, length_s),
        *result = (int *)calloc(size, sizeof(int));

    for (int k = 0, q = 0; k < length_t; ++k) {
        while (q > 0 && s[q] != t[k])
            q = p[q - 1];

        q += (s[q] == t[k]);

        if (q == length_s) {
            k = k - length_s + 1;
            result[m++] = k;
            if (m >= size) {
                size *= 2;
                result = (int *)realloc(result, size*sizeof(int));
            }
            q = 0;
        }
    }

    result[m] = -1;
    free(p);

    return result;
}

int main(int argc, char * argv[]) {
    if (argc != 3) {
        printf("Usage: %s <S> <T>\n", argv[0]);
        return -1;
    }

    size_t n = strlen(argv[1]);
    int * r = kmpall(argv[1], argv[2]);

    for (int i = 0; r[i] != -1; ++i)
        printf("%d ", r[i]);
    printf("\n");

    free(r);

    return 0;
}