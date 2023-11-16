#include <stdio.h>
#include <stdlib.h>

typedef struct node {
    struct node *l, *r;
    int k, v;
} node_t;

node_t * search(node_t * t, int k) {
    return t == NULL ? NULL :
           t->k == k ? t : 
           k < t->k ? search(t->l, k) : search(t->r, k);
}

int get(node_t * t, int k) {
    node_t * n = search(t, k);
    return n ? n->v : 0;
}

void insert(node_t * t, node_t * n) {
    return n->k < t->k ? 
           t->l ? insert(t->l, n) : ( t->l = n ) :
           t->r ? insert(t->r, n) : ( t->r = n );
}

void increase(node_t * t, int k) {
    node_t * n;
    if (n = search(t, k))
        return (void)n->v++;

    n = calloc(1, sizeof(node_t));
    n->k = k, n->v = 1;
    insert(t, n);
}

void delete(node_t * t) {
    return t ? (delete(t->l), delete(t->r), free(t)) : NULL;
}

int main(void) {
    node_t *t = calloc(1, sizeof(node_t));

    int n;
    scanf("%d", &n);

    unsigned int a[n];
    for (int i = 0; i < n; i++)
        scanf("%u", &a[i]);

    unsigned int s = 0, count = 0;

    for (int i = 0; i < n; i++) {
        s ^= a[i];
        if (s == 0)
            count += 1;

        count += get(t, s);
        increase(t, s);
    }

    printf("%u\n", count);

    delete(t);

    return 0;
}
