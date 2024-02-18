#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SWAP(a, b)         \
    {                      \
        char *t = a->word; \
        a->word = b->word; \
        b->word = t;       \
    }

struct Elem {
    struct Elem *next;
    char *word;
};

struct Elem *bsort(struct Elem *list) {
    struct Elem *temp = list;
    int n = 0;

    while (temp != NULL) {
        temp = temp->next;
        n++;
    }

    for (int i = 0; i < n-1; i++) {
        temp = list;
        for (int j = 0; j < n-i-1; j++) {
            if (strlen(temp->word) > strlen(temp->next->word))
                SWAP(temp, temp->next);

            temp = temp->next;
        }
    }

    return list;
}

void insert(struct Elem **list, char *word) {
    struct Elem *el = malloc(sizeof(struct Elem));
    el->next = NULL;
    el->word = word;
    
    if (*list == NULL) {
        *list = el;
    } else {
        struct Elem *temp = *list;

        while (temp->next != NULL)
            temp = temp->next;

        temp->next = el;
    }
}

void delete(struct Elem *list) {
    if (list == NULL)
        return;

    delete(list->next);
    free(list);
}

int main() {
    char *s = malloc(1001);
    fgets(s, 1000, stdin);
    s[strlen(s)-1] = '\0';

    struct Elem *list = NULL;

    char *p = strtok(s, " ");
    while (p) {
        insert(&list, p);
        p = strtok(NULL, " ");
    }

    bsort(list);

    struct Elem *temp = list;
    while (temp) {
        printf("%s\n", temp->word);
        temp = temp->next;
    }

    delete(list);
    free(s);

    return 0;
}