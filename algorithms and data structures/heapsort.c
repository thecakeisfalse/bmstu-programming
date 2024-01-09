#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#define BUFFER_SIZE 1000

int compare(const void * a, const void * b) {
    char *sa = *(char **)a, *sb = *(char **)b;
    int count = 0;

    for (size_t i = 0; i < strlen(sa); ++i)
        count += sa[i] == 'a';

    for (size_t i = 0; i < strlen(sb); ++i)
        count -= sb[i] == 'a';

    return count;
}

void heapify(void *base, size_t nel, size_t width, int (*compare)(const void *a, const void *b), size_t i) {
    void *bi = malloc(width);
    uint8_t *base_ = base;

    while (1) {
        size_t l = 2*i+1, r = 2*i+2, j = i;

        if (l < nel && compare((void*)(base_+i*width), (void*)(base_+l*width)) < 0) i = l;
        if (r < nel && compare((void*)(base_+i*width), (void*)(base_+r*width)) < 0) i = r;
        if (i == j) break;

        memcpy(bi, (void*)(base_+i*width), width);
        memcpy((void*)(base_+i*width), (void*)(base_+j*width), width);
        memcpy((void*)(base_+j*width), bi, width);
    }

    free(bi);
}

void buildheap(void *base, size_t nel, size_t width, int (*compare)(const void *a, const void *b)) {
    for (int i = nel / 2 - 1; i >= 0; --i)
        heapify(base, nel, width, compare, i);
}

void hsort(void *base, size_t nel, size_t width, int (*compare)(const void *a, const void *b)) {
    void *bi = malloc(width);
    uint8_t *base_ = base;

    buildheap(base, nel, width, compare);

    for (size_t i = nel - 1; i > 0; --i) {
        memcpy(bi, base_+i*width, width);
        memcpy(base_+i*width, base, width);
        memcpy(base, bi, width);
        heapify(base, i, width, compare, 0);
    }

    free(bi);
}

char * read_line() {
    int buffer_size = BUFFER_SIZE, pos = 0;
    char * buffer = (char *)calloc(buffer_size, sizeof(char)), symb;

    while (1) {
        symb = getchar();
        if (symb == EOF || symb == '\n') {
            buffer[pos] = '\0';
            break;
        }

        buffer[pos++] = symb;

        if (pos >= buffer_size) {
            buffer_size += BUFFER_SIZE;
            buffer = (char *)realloc(buffer, buffer_size);
        }
    }

    return buffer;
}

int main() {
    int n; scanf("%d\n", &n);
    char * s[n];
    for (int i = 0; i < n; ++i)
        s[i] = read_line();

    hsort(s, n, sizeof(char*), compare);

    for (int i = 0; i < n; ++i)
        printf("%s\n", s[i]);

    for (int i = 0; i < n; ++i)
        free(s[i]);

    return 0;
}