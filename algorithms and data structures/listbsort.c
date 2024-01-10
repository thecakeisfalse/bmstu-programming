#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1000
#define WORDS_SIZE 100

#define swap(a, b)         \
    {                      \
        char *temp = b->v; \
        b->v = a->v;       \
        a->v = temp;       \
    }

struct Elem {
    struct Elem *next;
    char * v;
};

struct Elem * bsort(struct Elem * list) {
    struct Elem *loop = list, *temp, *p1, *p2;

    for (; loop != NULL; loop = loop->next) {
        for (temp = list; temp->next != NULL; temp = temp->next) {
            p2 = temp->next, p1 = temp;
            if (p2 && strlen(p1->v) > strlen(p2->v))
                swap(p1, p2);
        }
    }
    return list;
}

void free_list(struct Elem * e) {
    if (e == NULL)
        return;

    free_list(e->next);
    free(e);
}

char ** split(char * str, char * delim) {
    int n = 0, size = WORDS_SIZE;
    char **words = (char **)calloc(size, sizeof(char *));

    for (char *s = strtok(str, delim); s != NULL; s = strtok(NULL, delim)) {
        int length = strlen(s);
        words[n] = (char *)calloc(length + 1, sizeof(char));
        memcpy(words[n++], s, length);

        if (n >= size) {
            size += WORDS_SIZE;
            words = realloc(words, sizeof(char *) * size);
        }
    }

    words[n] = NULL;

    return words;
}

char * read_line(void) {
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
    char * s = read_line();
    char ** words = split(s, " ");
    struct Elem * root = (struct Elem *)calloc(1, sizeof(struct Elem));
    struct Elem * cur = root;

    for (int i = 0; *(words + i) != NULL; ++i, cur = cur->next) {
        cur->next = (struct Elem *)calloc(1, sizeof(struct Elem));
        cur->next->v = words[i];
    }

    bsort(root->next);

    for (struct Elem * cur = root->next; cur != NULL; cur = cur->next)
        printf("%s ", cur->v);

    free_list(root);

    for (int i = 0; *(words+i) != NULL; ++i)
        free(*(words+i));
    
    free(words);
    free(s);

    return 0;
}