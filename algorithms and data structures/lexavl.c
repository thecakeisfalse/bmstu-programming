#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define INF __INT32_MAX__

typedef enum {
    CONST = 0,
    SPEC,
    IDENT,
} lexem_type;

typedef char *node_key_t;
typedef lexem_type value_t;

typedef struct node {
    node_key_t k;
    value_t v;
    int balance;
    struct node *parent;
    struct node *left, *right;
} node_t;

void *memdup(const void *mem, size_t size) {
    void *out = malloc(size);
    return out ? memcpy(out, mem, size) : NULL;
}

int spec2i(char c) {
    switch (c) {
    case '+':
        return 0;
    case '-':
        return 1;
    case '*':
        return 2;
    case '/':
        return 3;
    case '(':
        return 4;
    case ')':
        return 5;
    }
    return -1;
}

node_t *create_empty_node(void) {
    node_t *n = (node_t *)calloc(1, sizeof(node_t));
    n->left = n->right = n->parent = NULL;
    n->v = n->balance = INF;
    return n;
}

node_t *minimum(node_t *T) {
    node_t *x = T;
    if (T != NULL)
        while (x->left != NULL)
            x = x->left;

    return x;
}

node_t *succ(node_t *x) {
    if (x->right != NULL)
        return minimum(x->right);

    node_t *y = x->parent;
    while (y != NULL && x == y->right)
        x = y, y = y->parent;

    return y;
}

node_t *descend(node_t *T, node_key_t k,
                int (*compare)(node_key_t x, node_key_t y)) {
    node_t *x = T;

    while (x != NULL && compare(x->k, k) != 0) {
        if (compare(k, x->k) < 0)
            x = x->left;
        else
            x = x->right;
    }

    return x;
}

node_t *insert(node_t *T, node_key_t k, value_t v,
               int (*compare)(node_key_t x, node_key_t y)) {
    node_t *y = create_empty_node();
    y->v = v;
    y->k = memdup(k, strlen(k) + 1);

    y->parent = y->left = y->right = NULL;

    if (T == NULL) {
        T = y;
    } else {
        node_t *x = T;
        while (1) {
            assert(compare(x->k, k) != 0);
            if (compare(k, x->k) < 0) {
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
    }

    return y;
}

void replace_node(node_t *T, node_t *x, node_t *y) {
    if (x == T) {
        T = y;
        if (y != NULL)
            y->parent = NULL;
    } else {
        node_t *p = x->parent;
        if (y != NULL)
            y->parent = p;
        if (p->left == x)
            p->left = y;
        else
            p->right = y;
    }
}

void delete(node_t *T, node_key_t k,
            int (*compare)(node_key_t x, node_key_t y)) {
    node_t *x = descend(T, k, compare);
    assert(x != NULL);

    if (x->left == NULL && x->right == NULL)
        replace_node(T, x, NULL);
    else if (x->left == NULL)
        replace_node(T, x, x->right);
    else if (x->right == NULL)
        replace_node(T, x, x->left);
    else {
        node_t *y = succ(x);
        replace_node(T, y, y->right);
        x->left->parent = y;
        y->left = x->left;
        x->right->parent = y;
        y->right = x->right;
        replace_node(T, x, y);
    }
}

void rotate_left(node_t *T, node_t *x) {
    node_t *y = x->right;
    assert(y != NULL);

    replace_node(T, x, y);
    node_t *b = y->left;

    if (b != NULL)
        b->parent = x;

    x->right = b;
    x->parent = y;
    y->left = x;

    --x->balance;
    if (y->balance > 0)
        x->balance -= y->balance;

    --y->balance;
    if (x->balance < 0)
        y->balance += x->balance;
}

void rotate_right(node_t *T, node_t *x) {
    node_t *y = x->left;
    assert(y != NULL);

    replace_node(T, x, y);
    node_t *b = y->right;

    if (b != NULL)
        b->parent = x;

    x->left = b;
    x->parent = y;
    y->right = x;

    ++x->balance;
    if (y->balance < 0)
        x->balance -= y->balance;

    ++y->balance;
    if (x->balance > 0)
        y->balance += x->balance;
}

void insert_avl(node_t *T, node_key_t k, value_t v,
                int (*compare)(node_key_t x, node_key_t y)) {
    node_t *a = insert(T, k, v, compare);
    a->balance = 0;
    while (1) {
        node_t *x = a->parent;
        if (x == NULL)
            break;

        if (a == x->left) {
            --x->balance;
            if (x->balance == 0)
                break;

            if (x->balance == -2) {
                if (a->balance == 1)
                    rotate_left(T, a);
                rotate_right(T, x);
                break;
            }
        } else {
            ++x->balance;
            if (x->balance == 0)
                break;

            if (x->balance == 2) {
                if (a->balance == -1)
                    rotate_right(T, a);
                rotate_left(T, x);
                break;
            }
        }
        a = x;
    }
}

void delete_tree(node_t *T) {
    if (T == NULL)
        return;

    if (T->left != NULL)
        delete_tree(T->left);

    if (T->right != NULL)
        delete_tree(T->right);

    free(T->k);
    free(T);
}

int main(void) {
    node_t *tree = create_empty_node(), *node;
    tree->k = calloc(1, sizeof(char));

    int n;
    scanf("%d\n", &n);

    char c, prev;
    char *temp = (char *)calloc(n + 1, sizeof(char));

    int i = 0;

    for (int i = 0, k = 0, m = 0; i <= n; ++i) {
        c = (i == n ? ' ' : getc(stdin));
        if (ispunct(c) || isspace(c)) {
            if (isalpha(temp[0])) {
                if ((node = descend(tree, temp, strcmp)) == NULL ||
                    strcmp(node->k, temp)) {
                    printf("IDENT %d\n", m);
                    insert_avl(tree, temp, m++, strcmp);
                } else {
                    printf("IDENT %d\n", node->v);
                }
            } else if (isdigit(temp[0]))
                printf("CONST %s\n", temp);

            if (ispunct(c))
                printf("SPEC %d\n", spec2i(c));

            memset(temp, 0, k);
            k = 0;

        } else
            temp[k++] = c;
    }

    free(temp);
    delete_tree(tree);

    return 0;
}