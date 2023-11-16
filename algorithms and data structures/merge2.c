#include <stdio.h>
#include <stdlib.h>

#define swap(a, b)        \
    {                     \
        node_t *temp = a; \
        a = b;            \
        b = temp;         \
    }

typedef struct node {
    int nlist, value;
    struct node *next;
} node_t;

typedef struct {
    node_t *head, *tail;
} list_t;

typedef struct pqueue {
    int count;
    node_t **heap;
} pqueue_t;

pqueue_t *init_pqueue(int n) {
    pqueue_t *q = malloc(sizeof(pqueue_t));
    q->count = 0;
    q->heap = calloc(n, sizeof(node_t *));
    return q;
}

node_t *create_node(int k, int v) {
    node_t *n = calloc(1, sizeof(node_t));
    n->nlist = k;
    n->value = v;
    return n;
}

void push_node(list_t *t, node_t *n) {
    if (t->head == NULL) {
        t->head = t->tail = n;
        return;
    }

    t->tail->next = n;
    t->tail = t->tail->next;
}

node_t *pop_node(list_t *t) {
    if (t->head == NULL)
        return NULL;

    node_t *temp = t->head;
    t->head = t->head->next;
    return temp;
}

void free_nodes(node_t * n) {
    if (n == NULL)
        return;

    free_nodes(n->next);
    free(n);
}

void free_pqueue(pqueue_t *q) {
    for (int i = 0; i < q->count; ++i)
        free(q->heap[i]);

    free(q->heap);
    free(q);
}

void siftDown(pqueue_t *q, int i) {
    while (1) {
        int l = 2 * i + 1, r = 2 * i + 2, j = l;

        if (l >= q->count)
            break;

        if (r < q->count && q->heap[r]->value < q->heap[l]->value)
            j = r;

        if (q->heap[i]->value <= q->heap[j]->value)
            break;

        swap(q->heap[i], q->heap[j]);
        i = j;
    }
}

void siftUp(pqueue_t *q, int i) {
    while (q->heap[i]->value < q->heap[(i - 1) / 2]->value) {
        swap(q->heap[i], q->heap[(i - 1) / 2]);
        i = (i - 1) / 2;
    }
}

node_t * extract_min(pqueue_t *q) {
    node_t * min = q->heap[0];
    q->heap[0] = q->heap[q->count - 1];
    q->count -= 1;
    siftDown(q, 0);
    return min;
}

void insert(pqueue_t *q, node_t * n) {
    q->count += 1;
    q->heap[q->count - 1] = n;
    siftUp(q, q->count - 1);
}

int main(void) {
    int k, v, n = 0;
    scanf("%d", &k);

    pqueue_t *q = init_pqueue(k);

    int sizes[k];
    for (int i = 0; i < k; ++i) {
        scanf("%d", &sizes[i]);
        n += sizes[i];
    }

    list_t *a[k];
    for (int i = 0; i < k; i++) {
        a[i] = calloc(1, sizeof(list_t));

        for (int j = 0; j < sizes[i]; j++) {
            scanf("%d", &v);
            push_node(a[i], create_node(i, v));
        }

        insert(q, pop_node(a[i]));
    }

    for (int i = 0; i < n; ++i) {
        node_t *m = extract_min(q);

        printf("%d\n", m->value);

        if (a[m->nlist]->head != NULL)
            insert(q, pop_node(a[m->nlist]));

        free(m);
    }

    for (int i = 0; i < k; i++)
        free(a[i]);

    free_pqueue(q);

    return 0;
}