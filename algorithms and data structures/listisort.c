#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Elem {
    struct Elem *prev, *next;
    int v;
};

struct Elem * create_elem(void) {
    struct Elem * elem = (struct Elem *)calloc(1, sizeof(struct Elem));
    elem->v = __INT32_MAX__;
    elem->next = elem->prev = elem;
    return elem;
}

void free_elem(struct Elem * elem) {
    for (struct Elem * temp = elem->next, *v; temp != elem; ) {
        v = temp;
        temp = temp->next;
        free(v);
    }

    free(elem);
}

void insertsort(int n, struct Elem * list) {
    struct Elem *i, *loc, *j;

    for (i = list->next; n --> 0; i = j) {
        j = i->next;
        
        for (loc = i->prev; loc != list && loc->v > i->v;)
            loc = loc->prev;

        i->prev->next = i->next;
        i->next->prev = i->prev;

        i->next = loc->next;
        i->prev = loc;

        loc->next->prev = i;
        loc->next = i;
    }
}

int main() {
    int n, m; scanf("%d", &n);

    struct Elem *root = create_elem(), *temp;

    for (temp = root, m = n; m --> 0; temp = temp->next) {
        temp->next = create_elem();
        scanf("%d", &temp->next->v);
        temp->next->next = root;
        temp->next->prev = temp;
    }

    insertsort(n, root);

    for (temp = root->next; n --> 0; temp = temp->next)
        printf("%d\n", temp->v);

    free_elem(root);

    return 0;
}
