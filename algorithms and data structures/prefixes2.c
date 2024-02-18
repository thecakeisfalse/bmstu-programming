#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    char *s = argv[1];
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

    for (int i = 0; i < n; i++) {
        t = i + 1 - pi[i];
        if ((i + 1) > t && (i + 1) % t == 0)
            printf("%d %d\n", i+1, (i+1) / t);
    }

    free(pi);

    return 0;
}