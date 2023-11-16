#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_CHAR 128

int max(int a, int b) {
    return (a > b) ? a : b;
}

void computeBadCharShift(char* pattern, int badCharShift[MAX_CHAR]) {
    int patternLength = strlen(pattern);
    for (int i = 0; i < MAX_CHAR; i++) {
        badCharShift[i] = patternLength;
    }
    for (int i = 0; i < patternLength - 1; i++) {
        badCharShift[(int)pattern[i]] = patternLength - i - 1;
    }
}

void computeSuffix(char* pattern, int suffix[]) {
    int patternLength = strlen(pattern);
    suffix[patternLength - 1] = patternLength - 1;

    int t = patternLength - 1;
    for (int i = patternLength - 2; i >= 0; i--) {
        while (t < patternLength - 1 && pattern[t] != pattern[i]) {
            t = suffix[t + 1];
        }

        if (pattern[t] == pattern[i]) {
            t -= 1;
        }

        suffix[i] = t;
    }
}

void computeGoodSuffixShift(char* pattern, int suffixShift[]) {
    int patternLength = strlen(pattern);
    int* suffix = calloc(patternLength + 1, sizeof(int));
    computeSuffix(pattern, suffix);

    for (int i = 0, t = suffix[0]; i < patternLength; i++) {
        while (t < i)
            t = suffix[t + 1];

        suffixShift[i] = -i + t + patternLength;
    }

    for (int i = 0; i < patternLength - 1; i++) {
        int t = i;
        while (t < patternLength - 1) {
            t = suffix[t + 1];
            if (pattern[t] != pattern[i]) {
                suffixShift[t] = -(i + 1) + patternLength;
            }
        }
    }

    free(suffix);
}

void searchBoyerMoore(char* text, char* pattern) {
    int textLength = strlen(text);
    int patternLength = strlen(pattern);

    int badCharShift[MAX_CHAR];
    computeBadCharShift(pattern, badCharShift);

    int *suffixShift = calloc(patternLength + 1, sizeof(int));
    computeGoodSuffixShift(pattern, suffixShift);

    int k = patternLength - 1;
    while (k < textLength) {
        int i = patternLength - 1;
        while (text[k] == pattern[i]) {
            if (i == 0) {
                printf("%d\n", k);
                break;
            }

            i--;
            k--;
        }

        k += max(badCharShift[(int)text[k]], suffixShift[i]);
    }

    free(suffixShift);
}

int main(int argc, char* argv[]) {
    if (argc != 3) {
        printf("Usage: %s <S> <T>\n", argv[0]);
        return 1;
    }

    char* pattern = argv[1]; // Исходная подстрока S
    char* text = argv[2];    // Строка T для поиска в ней подстроки

    searchBoyerMoore(text, pattern);

    return 0;
}