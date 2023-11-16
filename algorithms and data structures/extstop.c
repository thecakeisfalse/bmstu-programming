#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int **stop_symbol_heuristic(char *s, int size) {
    int length = strlen(s);

    int **delta1 = (int **)calloc(length, sizeof(int *));

    for (int i = 0; i < length; ++i) {
        delta1[i] = (int *)calloc(size, sizeof(int));
        for (int j = 0; j < size; ++j)
            delta1[i][j] = length;
    }

    for (int i = 0; i < length; ++i) {
        if (i > 0)
            memcpy(delta1[i], delta1[i - 1], size * sizeof(int));

        delta1[i][(int)s[i]] = length - 1 - i;
    }

    return delta1;
}

int bmsubst(char *s, int size, char *t) {
    int length_s = strlen(s), length_t = strlen(t), k, i, result = length_t;

    bool quit = false;

    int **delta1 = stop_symbol_heuristic(s, size);

    for (k = length_s - 1, i = k; k < length_t && !quit;
         k += fmax(delta1[i][(int)t[k]], length_s - i)) {
        for (i = length_s - 1; t[k] == s[i]; --i, --k) {
            if (i == 0) {
                quit = true;
                result = k;
                break;
            }
        }
    }

    for (int i = 0; i < length_s; ++i)
        free(delta1[i]);
    free(delta1);

    return result;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        printf("Usage: %s <S> <T>\n", argv[0]);
        return -1;
    }

    printf("%d\n", bmsubst(argv[1], 127, argv[2]));

    return 0;
}