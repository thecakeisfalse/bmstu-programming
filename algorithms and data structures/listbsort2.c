#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct elem {
    struct elem *next;
    char *word;
} elem;

elem *bsort(elem *list) {
    elem *temp, *cycle, *first, *second;

    cycle = list;
    while (cycle) {
        temp = list;
        while (temp->next) {
            first = temp;
            second = temp->next;

            if (strlen(first->word) > strlen(second->word)) {
                char *ptr = first->word;
                first->word = second->word;
                second->word = ptr;
            }

            temp = temp->next;
        }
        cycle = cycle->next;
    }

    return list;
}

int main() {
    char *s = malloc(1001);
    fgets(s, 1000, stdin);
    s[strlen(s)-1] = '\0';

    elem *list = calloc(1, sizeof(elem)), *temp = list;

    char *p = strtok(s, " ");
    while (p) {
        temp->word = p;
        p = strtok(NULL, " ");
        temp->next = (p ? calloc(1, sizeof(elem)) : NULL);
        temp = temp->next;
    }

    bsort(list);

    while (list) {
        printf("%s\n", list->word);
        temp = list->next;
        free(list);
        list = temp;
    }

    free(s);

    return 0;
}