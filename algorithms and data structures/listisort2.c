#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct Elem {
    struct Elem *prev, *next;
    int v;
};

void insert(struct Elem **root, int v) {
    struct Elem *el = malloc(sizeof(struct Elem));
    el->v = v;

    if (*root == NULL) {
        el->next = el->prev = el;
        *root = el;
    } else {
        struct Elem *last = (*root)->prev;
        el->next = *root;
        (*root)->prev = el;
        el->prev = last;
        last->next = el;
    }
}

void delete(struct Elem *root) {
    struct Elem *temp = root->next;
    while (temp != root) {
        struct Elem *next = temp->next;
        free(temp);
        temp = next;
    }
    free(root);
}

void insertsort(int n, struct Elem *list) {
    struct Elem *i_elem = list->next, *i_next;

    for (int i = 1; i < n; i++) {
        i_next = i_elem->next;
        int loc = i-1, elem = i_elem->v;
        struct Elem *loc_elem = i_elem->prev;

        while (loc >= 0 && loc_elem->v > elem) {
            loc_elem = loc_elem->prev;
            loc--;
        }
        
        i_elem->prev->next = i_elem->next;
        i_elem->next->prev = i_elem->prev;

        i_elem->next = loc_elem->next;
        i_elem->prev = loc_elem;

        loc_elem->next->prev = i_elem;
        loc_elem->next = i_elem;

        i_elem = i_next;
    }
}

int main() {
    int n, m;
    scanf("%d", &n);

    struct Elem *root = NULL;

    insert(&root, 1e9);
    for (int i = 0; i < n; i++) {
        int x;
        scanf("%d", &x);
        insert(&root, x);
    }

    insertsort(n, root->next);

    struct Elem *temp = root->next;
    for (int i = 0; i < n; i++) {
        printf("%d\n", temp->v);
        temp = temp->next;
    }

    delete(root);

    return 0;
}
