#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INF __INT32_MAX__
#define COMPARE(x, y) ((int64_t)x - (int64_t)y)

typedef int node_key_t;
typedef char *node_value_t;

typedef struct node {
    node_key_t k;
    node_value_t v;
    int count;
    struct node *parent;
    struct node *left, *right;
} node_t;

void *memdup(const void *mem, size_t size) {
    void *out = malloc(size);
    return out ? memcpy(out, mem, size) : NULL;
}

node_t *create_empty_node(void) {
    node_t *n = (node_t *)calloc(1, sizeof(node_t));

    n->left = n->right = n->parent = NULL;
    n->k = INF;
    n->v = NULL;
    n->count = 0;

    return n;
}

node_t *create_node(node_key_t k, node_value_t v) {
    node_t *n = create_empty_node();

    n->k = k;
    n->v = memdup(v, strlen(v) + 1);

    return n;
}

void delete_node(node_t *n) {
    free(n->v);
    free(n);
}

void delete_tree(node_t *T) {
    if (T == NULL)
        return;

    delete_tree(T->left);
    delete_tree(T->right);
    delete_node(T);
}

node_t *minimum(node_t *T) {
    node_t *x = T;
    if (x != NULL)
        while (x->left != NULL)
            x = x->left;

    return x;
}

node_t *succ(node_t *x) {
    if (x->right != NULL)
        return minimum(x->right);

    node_t *y = x->parent;

    while (y != NULL && y->right == x)
        x = y, y = y->parent;

    return y;
}

node_t *descend(node_t *T, node_key_t k) {
    node_t *x = T;

    while (x != NULL && COMPARE(x->k, k) != 0)
        x = (COMPARE(k, x->k) < 0 ? x->left : x->right);

    return x;
}

node_t *insert(node_t *T, node_key_t k, node_value_t v) {
    node_t *y = create_node(k, v);
    if (T == NULL)
        return y;

    node_t *x = T;
    while (1) {
        assert(COMPARE(x->k, k) != 0);

        if (COMPARE(k, x->k) < 0) {
            ++x->count;
            if (x->left == NULL) {
                x->left = y;
                y->parent = x;
                break;
            }
            x = x->left;
        } else {
            if (x->right == NULL) {
                x->right = y;
                y->parent = x;
                break;
            }
            x = x->right;
        }
    }

    return T;
}

node_t *replace_node(node_t *T, node_t *x, node_t *y) {
    if (x == T) {
        T = y;
        if (y != NULL)
            y->parent = NULL;
    } else {
        node_t *p = x->parent;
        if (y != NULL)
            y->parent = p;
        if (p->right == x)
            p->right = y;
        else
            p->left = y;
    }

    return T;
}

void decrement(node_t *n) {
    for (node_t *x = n; x != NULL && x->parent != NULL; x = x->parent)
        x->parent->count -= (x->parent->left == x);
}

node_t *delete(node_t *T, node_key_t k) {
    node_t *x = descend(T, k);
    assert(x != NULL);

    if (x->left != NULL && x->right != NULL) {
        node_t *y = succ(x);

        decrement(y);

        T = replace_node(T, y, y->right);
        T = replace_node(T, x, y);

        y->left = x->left;
        y->left->parent = y;

        if (x->right != NULL) {
            y->right = x->right;
            y->right->parent = y;
        }

        y->count = x->count;
    } else {
        decrement(x);

        if (x->left == NULL && x->right == NULL)
            T = replace_node(T, x, NULL);
        else
            T = replace_node(T, x, (x->left ? x->left : x->right));
    }

    delete_node(x);

    return T;
}

node_t *searchbyrank(node_t *T, int rank) {
    node_t *x = T;

    while (rank != x->count) {
        if (rank > x->count) {
            rank -= x->count + 1;
            x = x->right;
        } else
            x = x->left;
    }

    return x;
}

int main(void) {
    node_t *tree = NULL;

    char cmd[7];
    while (1) {
        scanf("%s", cmd);

        if (!strcmp(cmd, "END")) {
            break;
        } else if (!strcmp(cmd, "LOOKUP")) {
            int k;
            scanf("%d", &k);
            printf("%s\n", descend(tree, k)->v);
        } else if (!strcmp(cmd, "INSERT")) {
            int k;
            char v[1000];
            scanf("%d %s", &k, v);
            tree = insert(tree, k, v);
        } else if (!strcmp(cmd, "DELETE")) {
            int k;
            scanf("%d", &k);
            tree = delete (tree, k);
        } else if (!strcmp(cmd, "SEARCH")) {
            int k;
            scanf("%d", &k);
            printf("%s\n", searchbyrank(tree, k)->v);
        }
    }

    delete_tree(tree);

    return 0;
}