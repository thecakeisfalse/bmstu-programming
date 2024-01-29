#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int *prefix(char *s, size_t n) {
    int *p = (int *)calloc(n, sizeof(int));

    for (int t = 0, i = 1; i < n; p[i++] = t) {
        while (t > 0 && s[t] != s[i])
            t = p[t - 1];

        t += (s[t] == s[i]);
    }

    return p;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <string>\n", argv[0]);
        return -1;
    }

    size_t n = strlen(argv[1]);
    int *p = prefix(argv[1], n);

    for (int i = 0; i < n; ++i) {
        if (p[i]) {
            int length = i + 1 - p[i];
            if ((i + 1) % length == 0) {
                printf("%d %d\n", p[i] + length, p[i] / length + 1);
            }
        }
    }

    free(p);

    return 0;
}