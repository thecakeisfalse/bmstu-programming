#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *concat(char **s, int n) {
    int length = 0;
    for (int i = 0; i < n; length += strlen(s[i]), i++)
        ;

    char *result = (char *)calloc(length + 1, sizeof(char));
    for (int i = 0, shift = 0; i < n; shift += strlen(s[i]), ++i)
        strcpy(result + shift, s[i]);

    return result;
}

int main() {
    int n;
    scanf("%d", &n);
    char *sarray[n];

    for (int i = 0; i < n; ++i) {
        sarray[i] = (char *)malloc(1001 * sizeof(char));
        strcpy(sarray[i], "");
        scanf("%s", sarray[i]);
    }

    char *result = concat(sarray, n);

    printf("%s\n", result);

    for (int i = 0; i < n; ++i) {
        free(sarray[i]);
    }

    free(result);

    return 0;
}
