#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <time.h>
#include <assert.h>

#define INF __INT32_MAX__
#define COMPARE(x, y) ((int64_t)x - (int64_t)y)

void * memdup(const void * mem, size_t size) { 
    void * out = malloc(size);
    memcpy(out, mem, size);
    return out;
}

#define MAX_LEVELS 30

typedef struct node {
    int k;
    char * v;
    struct node ** next;
    int * span;
} node_t;

node_t * create_node(int k, char * v) {
    node_t * x = (node_t *)calloc(1, sizeof(node_t));
    x->k = k;
    x->v = memdup(v, strlen(v) + 1);
    x->next = (node_t **)calloc(MAX_LEVELS, sizeof(node_t *));
    x->span = (int *)calloc(MAX_LEVELS, sizeof(int));
    return x;
}

node_t * create_list(void) {
    node_t * n = (node_t *)calloc(1, sizeof(node_t));
    n->k = -INF;
    n->next = (node_t **)calloc(MAX_LEVELS, sizeof(node_t *));
    n->span = (int *)calloc(MAX_LEVELS, sizeof(int));

    for (int i = 0; i < MAX_LEVELS; ++i)
        n->next[i] = NULL;

    return n;
}

node_t ** skip(node_t * l, int k) {
    node_t ** p = (node_t **)calloc(MAX_LEVELS, sizeof(node_t *)), *x = l;

    for (int i = MAX_LEVELS-1; i >= 0; --i) {
        while (x->next[i] != NULL && COMPARE(x->next[i]->k, k) < 0)
            x = x->next[i];

        p[i] = x;
    }

    return p;
}

node_t * search(node_t * l, int k) {
    node_t ** p = skip(l, k);
    node_t * x = p[0]->next[0];
    free(p);
    return (x == NULL || COMPARE(x->k, k) != 0 ? NULL : x);
}

int rank(node_t * l, int k) {
    int r = 0;
    node_t * x = l;

    for (int m = MAX_LEVELS - 1; m >= 0; --m) {
        while (x->next[m] != NULL && COMPARE(x->next[m]->k, k) < 0) {
            r += x->span[m];
            x = x->next[m];
        }
    }

    return (k == -INF ? -1 : r);
}

void insert(node_t * l, int k, char * v) {
    node_t ** p = skip(l, k);
    node_t * x = create_node(k, v);

    uint64_t i = 0,
             r = rand() * 2LL,
             rank_x = rank(l, p[0]->k) + 1;

    for (; i < MAX_LEVELS && r % 2 == 0; ++i, r /= 2) {
        x->next[i] = p[i]->next[i];
        p[i]->next[i] = x;

        int temp_rank = rank(l, p[i]->k);
        x->span[i] = p[i]->span[i] + 1 - (rank_x - temp_rank);
        p[i]->span[i] = rank_x - temp_rank;
    }

    for (; i < MAX_LEVELS; ++i) {
        x->next[i] = NULL;
        ++p[i]->span[i];
    }

    free(p);
}

void free_node(node_t * n) {
    if (n == NULL)
        return;

    free_node(n->next[0]);
    free(n->next);
    free(n->span);
    free(n->v);
    free(n);
}

void delete(node_t * l, int k) {
    node_t ** p = skip(l, k);
    node_t * x = p[0]->next[0];

    assert(x != NULL && COMPARE(x->k, k) == 0);

    int i = 0;
    for (; i < MAX_LEVELS && p[i]->next[i] == x; ++i) {
        p[i]->next[i] = x->next[i];
        p[i]->span[i] += x->span[i] - 1;
    }

    for (; i < MAX_LEVELS; ++i)
        --p[i]->span[i];

    free(x->next);
    free(x->span);
    free(x->v);
    free(x);

    free(p);
}

int main(void) {
    srand(time(NULL));

    node_t * l = create_list();

    char cmd[7];
    while (1) {
        scanf("%s", cmd);

        if (!strcmp(cmd, "END")) {
            break;
        } else if (!strcmp(cmd, "LOOKUP")) {
            int k; scanf("%d", &k);
            printf("%s\n", search(l, k)->v);
        } else if (!strcmp(cmd, "INSERT")) {
            int k; char v[1000]; scanf("%d %s", &k, v);
            insert(l, k, v);
        } else if (!strcmp(cmd, "DELETE")) {
            int k; scanf("%d", &k);
            delete(l, k);
        } else if (!strcmp(cmd, "RANK")) {
            int k; scanf("%d", &k);
            printf("%d\n", rank(l, k));
        }
    }

    free_node(l);

    return 0;
}