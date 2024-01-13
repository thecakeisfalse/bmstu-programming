#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct Node {
    int k, v;
    struct Node * next_node;
};

typedef struct Node Node_t;

int hash(int i, int m) {
    return i % m;
}

Node_t * new_node(int k, int v) {
    Node_t * node = (Node_t *)calloc(1, sizeof(Node_t));

    node->k = k;
    node->v = v;
    node->next_node = NULL;

    return node;
}

Node_t * search(Node_t * node, int k) {
    Node_t * cur = node;

    while (cur != NULL && cur->k != k)
        cur = cur->next_node;

    return cur;
}

void insert(Node_t * node, int k, int v) {
    Node_t *cur = node, *prev = NULL;

    assert(cur != NULL);

    for (; cur->next_node != NULL && cur->k != k; prev = cur, cur = cur->next_node);

    if (v == 0) {
        if (cur->k == k) {
            prev->next_node = cur->next_node;
            free(cur);
        }

        return;
    }

    if (cur->k == k)
        cur->v = v;
    else
        cur->next_node = new_node(k, v);
}

void delete(Node_t * node) {
    if (node == NULL)
        return;
    
    delete(node->next_node);
    free(node);
}

int main() {
    int m; scanf("%d", &m);

    Node_t ** array = (Node_t **)calloc(m, sizeof(Node_t*));
    for (int i = 0; i < m; ++i)
        array[i] = new_node(-1, -1);

    char cmd[7];
    while (1) {
        scanf("%s", cmd);
        if (cmd[1] == 'N') {
            break;
        } else if (cmd[1] == 'T') {
            int i; scanf("%d", &i);
            Node_t * cur = search(array[hash(i, m)], i);
            printf("%d\n", (cur == NULL ? 0 : cur->v));
        } else if (cmd[1] == 'S') {
            int k, v; scanf("%d %d", &k, &v);
            insert(array[hash(k, m)], k, v);
        }
    }

    for (int i = 0; i < m; ++i)
        delete(array[i]);
    free(array);

    return 0;
}