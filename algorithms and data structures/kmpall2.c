#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int *prefix(char *s) {
    int n = strlen(s);
    int *pi = malloc(n * sizeof(int));
    pi[0] = 0;

    int t = 0, i = 1;
    while (i < n) {
        while (t > 0 && s[t] != s[i])
            t = pi[t - 1];

        if (s[t] == s[i])
            t += 1;

        pi[i] = t;
        i += 1;
    }

    return pi;
}

void kmpall(char *s, char *t) {
    int *pi = prefix(s);
    int q = 0, k = 0;
    int nt = strlen(t), ns = strlen(s);

    while (k < nt) {
        while (q > 0 && s[q] != t[k])
            q = pi[q - 1];
        
        if (s[q] == t[k])
            q += 1;

        if (q == ns) {
            k = k - ns + 1;
            printf("%d\n", k);
            q = 0;
        }

        k += 1;
    }

    free(pi);
}

int main(int argc, char *argv[]) {
    kmpall(argv[1], argv[2]);
    return 0;
}