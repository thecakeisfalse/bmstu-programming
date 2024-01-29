#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int *prefix(char *s, size_t n) {
    int *p = (int *)calloc(n, sizeof(int));

    for (size_t t = 0, i = 1; i < n; p[i++] = t) {
        while (t > 0 && s[t] != s[i])
            t = p[t - 1];

        t += (s[t] == s[i]);
    }

    return p;
}

int pword(char *s, char *t) {
    int length_s = strlen(s), length_t = strlen(t);

    char *d = (char *)calloc(length_s + length_t + 1, sizeof(char));

    memcpy(d, s, length_s);
    memcpy((void *)((size_t)d + length_s), t, length_t);

    int *table = prefix(d, length_s + length_t);

    int fl = 1;
    for (int i = 0; i < length_t; ++i)
        fl = fl && table[i + length_s] != 0;

    free(d);
    free(table);
    return fl;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Usage: %s <S> <T>\n", argv[0]);
        return -1;
    }

    printf("%s\n", (pword(argv[1], argv[2]) ? "yes" : "no"));

    return 0;
}