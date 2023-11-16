#include <stdio.h>
#include <stdlib.h>

#define MAX_LEVELS 30
#define INF __INT32_MAX__

typedef struct node {
    int k, v;
    struct node **next;
} __attribute__((packed)) node_t;

node_t *create_list(void) {
    node_t *list = malloc(sizeof(node_t));

    list->k = INF;
    list->next = calloc(MAX_LEVELS, sizeof(node_t *));

    return list;
}

node_t **skip(node_t *list, int k) {
    node_t **p = malloc(MAX_LEVELS * sizeof(node_t *)), *x = list;

    for (int i = MAX_LEVELS - 1; i >= 0; i--) {
        while (x->next[i] != NULL && x->next[i]->k < k)
            x = x->next[i];

        p[i] = x;
    }

    return p;
}

node_t *lookup(node_t *list, int k) {
    node_t *x = list;

    for (int i = MAX_LEVELS - 1; i >= 0; i--)
        while (x->next[i] != NULL && x->next[i]->k < k)
            x = x->next[i];

    x = x->next[0];

    return (x == NULL || x->k != k ? NULL : x);
}

node_t *insert(node_t *list, int k, int v) {
    node_t *x = create_list();
    x->k = k;
    x->v = v;

    node_t **p = skip(list, k);

    long long i = 0, r = rand() * 2;

    for (; i < MAX_LEVELS && r % 2 == 0; i++) {
        x->next[i] = p[i]->next[i];
        p[i]->next[i] = x;
        r /= 2;
    }

    for (; i < MAX_LEVELS; ++i)
        x->next[i] = NULL;

    free(p);

    return x;
}

void delete(node_t *list) {
    if (list == NULL)
        return;

    delete (list->next[0]);
    free(list->next);
    free(list);
}

int main(void) {
    node_t *list = create_list(), *node;

    int n;
    scanf("%d", &n);

    unsigned int s = 0, x, count = 0;

    for (int i = 0; i < n; i++) {
        scanf("%d", &x);
        s ^= x;
        if ((node = lookup(list, s)) == NULL)
            node = insert(list, s, 0);

        count += (s == 0);
        count += node->v;
        node->v += 1;
    }

    printf("%u\n", count);

    delete (list);

    return 0;
}