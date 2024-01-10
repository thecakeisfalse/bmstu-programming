#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int * stop_symbol_heuristic(char * s, int size) {
    int length = strlen(s);
    int * delta1 = (int *)calloc(size, sizeof(int));

    for (int i = 0; i < size; ++i)
        delta1[i] = length;

    for (int i = 0; i < length; ++i)
        delta1[(int)s[i]] = length - 1 - i;

    return delta1;
}

int * suffix(char * s) {
    int length = strlen(s);
    int * sigma = (int *)calloc(length+1, sizeof(int));
    sigma[length-1] = length-1;

    for (int t = length-1, i = length-2; i >= 0; sigma[i--] = t) {
        while (t < length - 1 && s[t] != s[i])
            t = sigma[t + 1];
        t -= (s[t] == s[i]);        
    }

    return sigma;
}

int * good_suffix_heuristic(char * s) {
    int length = strlen(s);
    int * delta2 = (int *)calloc(length+1, sizeof(int));
    int * sigma = suffix(s);

    for (int i = 0, t = sigma[0]; i < length; ++i) {
        while (t < i)
            t = sigma[t + 1];
        delta2[i] = -i + t + length;
    }

    for (int i = 0, t = i; i < length - 1; ++i) {
        for (t = i; t < length - 1; ) {
            t = sigma[t + 1];
            if (s[t] != s[i])
                delta2[t] = -(i + 1) + length;
        }
    }

    free(sigma);

    return delta2;
}

int * bmall(char * s, int size, char * t) {
    int length_s = strlen(s),
        length_t = strlen(t),
        res_size = 10, m = 0;

    int * delta1 = stop_symbol_heuristic(s, size),
        * delta2 = good_suffix_heuristic(s),
        * result = (int *)calloc(size, sizeof(int));

    for (int k = length_s-1, i = k; k < length_t; k += fmax(delta1[(int)t[k]], delta2[i])) {
        for (i = length_s - 1; t[k] == s[i]; --i, --k) {
            if (i == 0) {
                result[m++] = k;
                if (m >= res_size) {
                    res_size *= 2;
                    result = (int *)realloc(result, res_size*sizeof(int));
                }
                break;
            }
        }
    }

    free(delta1);
    free(delta2);

    result[m] = -1;

    return result;
}

int main(int argc, char * argv[]) {
    if (argc != 3) {
        printf("Usage: %s <S> <T>\n", argv[0]);
        return -1;
    }

    int * result = bmall(argv[1], 127, argv[2]);

    for (int i = 0; result[i] != -1; ++i)
        printf("%d ", result[i]);
    printf("\n");

    free(result);

    return 0;
}