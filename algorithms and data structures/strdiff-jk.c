#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int strdiff(char * a, char * b) {
    size_t na = strlen(a), nb = strlen(b), i = 0, j = 0;
    for (i = 0; i < na && i < nb && a[i] == b[i]; ++i);
    if (i == na && i == nb) return -1;
    for (j = 0; j <= 7 && ((a[i] ^ b[i]) & (1 << j)) == 0; ++j);
    return i * 8 + j;
}

int main() {
    char *a = (char *)calloc(1024, sizeof(char)),
         *b = (char *)calloc(1024, sizeof(char));
    scanf("%s%s", a, b);
    printf("%d\n", strdiff(a, b));
    free(a);
    free(b);

    return 0;
}
