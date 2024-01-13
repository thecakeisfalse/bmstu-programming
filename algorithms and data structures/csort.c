#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define BUFFER_SIZE 1000
#define WORDS_SIZE 100

typedef struct {
    char * word;
    int length;
} __attribute__((packed)) word;

typedef struct {
    word ** words;
    size_t n;
} __attribute__((packed)) split_res;

static char * read_line(void) {
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

static split_res * split(char *src, char delim) {
    size_t length = strlen(src), size = WORDS_SIZE;

    split_res * res = (split_res *)calloc(1, sizeof(split_res));

    res->words = (word **)calloc(size, sizeof(word *));

    for (size_t i = 0; i < length; ++i) {
        res->n += (src[i] == delim &&
                   res->words[res->n] != NULL &&
                   res->words[res->n]->length > 0);

        if (res->n >= size) {
            size += WORDS_SIZE;
            res->words = realloc(res->words, sizeof(word*) * size);

            for (int i = res->n; i < size; ++i)
                res->words[i] = NULL;
        }

        if (src[i] == delim)
            continue;

        if (res->words[res->n] == NULL) {
            res->words[res->n] = (word *)calloc(1, sizeof(word));
            res->words[res->n]->word = (char *)calloc(fmin(BUFFER_SIZE, length), sizeof(word));
        }

        res->words[res->n]->word[res->words[res->n]->length++] = src[i];
    }

    res->n += length > 0 && src[length-1] != delim;

    return res;
}

static void free_split(split_res * res) {
    for (size_t i = 0; i < res->n; ++i) {
        free(res->words[i]->word);
        free(res->words[i]);
    }

    free(res->words);
    free(res);
}

void csort(char *src, char *dest) {
    split_res * res = split(src, ' ');

    if (res->n == 0) {
        dest[0] = '\0';
        free_split(res);
        return;
    }

    int * count = (int *)calloc(sizeof(int), res->n);
    size_t shift = 0;

    for (int j = 0; j < res->n - 1; ++j) {
        for (int i = j + 1; i < res->n; ++i) {
            if (res->words[i]->length < res->words[j]->length)
                ++count[j];
            else
                ++count[i];
        }
    }

    int * o = (int *)calloc(sizeof(int), res->n);

    for (int i = 0; i < res->n; ++i)
        o[count[i]] = i;

    word * w;
    for (int i = 0; i < res->n; shift += w->length+1, ++i) {
        w = res->words[o[i]];
        memset(dest+shift, ' ', w->length+1);
        memcpy(dest+shift, w->word, w->length);
    }

    dest[shift-1] = '\0';
  
    free_split(res);
    free(count);
    free(o);
}

int main() {
    char * src = read_line();
    char * dest = (char *)calloc(strlen(src)+1, sizeof(char));

    csort(src, dest);

    puts(dest);

    free(dest);
    free(src);

    return 0;
}