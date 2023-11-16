#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define HASH(i, m) i % m

typedef struct node {
    struct node *next, *prev;
    int k, v;
} node_t;

typedef struct {
    int m;
    node_t **n;
} disparray_t;

node_t *create_node(int k, int v, node_t * prev) {
    node_t *n = calloc(1, sizeof(node_t));

    n->k = k;
    n->v = v;
    n->prev = prev;

    return n;
}

void delete_node(node_t *n) {
    if (n == NULL)
        return;

    if (n->prev != NULL)
        n->prev->next = n->next;
    
    if (n->next != NULL)
        n->next->prev = n->prev;

    free(n);
}

disparray_t *create_disparray(int m) {
    disparray_t *arr = malloc(sizeof(disparray_t));

    arr->m = m;
    arr->n = malloc(m * sizeof(node_t*));

    for (int i = 0; i < m; i++)
        arr->n[i] = create_node(-1, 0, NULL);

    return arr;
}

node_t * search(node_t * t, int k) {
    return t == NULL ? NULL :
           t->k == k ? t : search(t->next, k);
}

int get(node_t * t, int k) {
    node_t * n = search(t, k);
    return n ? n->v : 0;
}

void insert(node_t * t, int k, int v) {
    return t->next ? insert(t->next, k, v) :
                    (t->next = create_node(k, v, t));
}

void set(node_t * t, int k, int v) {
    node_t * n = search(t, k);

    return v == 0 ? delete_node(n) :
           n == NULL ? insert(t, k, v) : (n->v = v);
}

void free_node(node_t * n) {
    if (n == NULL)
        return;

    free_node(n->next);
    free(n);
}

void free_disparray(disparray_t *arr) {
    for (int i = 0; i < arr->m; i++)
        free_node(arr->n[i]);

    free(arr->n);
    free(arr);
}

int main() {
    int m;
    scanf("%d", &m);

    disparray_t *arr = create_disparray(m);

    char s[7];
    while (1) {
        scanf("%s", s);

        if (!strcmp(s, "END"))
            break;
        
        if (!strcmp(s, "AT")) {
            int k;
            scanf("%d", &k);
            printf("%d\n", get(arr->n[HASH(k, m)], k));
        } else if (!strcmp(s, "ASSIGN")) {
            int k, v;
            scanf("%d %d", &k, &v);
            set(arr->n[HASH(k, m)], k, v);
        }
    }

    free_disparray(arr);

    return 0;
}