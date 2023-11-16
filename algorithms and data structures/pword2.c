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

int is_prefix(int *pi, int ns, int nt) {
    for (int i = 0; i < nt; i++)
        if (pi[i + ns] == 0)
            return 0;

    return 1;
}

int main(int argc, char *argv[]) {
    char *s = argv[1], *t = argv[2];
    int ns = strlen(s), nt = strlen(t);
    char *d = calloc(ns+nt+1, 1);
    d = strcat(strcat(d, s), t);
    int *pi = prefix(d);

    if (is_prefix(pi, ns, nt))
        printf("yes\n");
    else
        printf("no\n");

    free(d);
    free(pi);

    return 0;
}