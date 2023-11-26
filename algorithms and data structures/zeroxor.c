#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct Node {
    int k, v;
    struct Node * parent;
    struct Node * left, * right;
};

struct Node * lookup(struct Node * T, int k) {
    struct Node * x = T;

    while (x != NULL && x->k != k) {
        if (k < x->k)
            x = x->left;
        else
            x = x->right;
    }

    return x;
}

void insert(struct Node * T, int k, int v) {
    struct Node * y = (struct Node *)calloc(sizeof(struct Node), 1);
    y->k = k;
    y->v = v;
    y->parent = y->left = y->right = NULL;

    if (T == NULL) {
        T = y;
    } else {
        struct Node * x = T;
        while (1) {
            assert (x->k != k);
            if (k < x->k) {
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
}

void delete(struct Node * T) {
    if (T == NULL)
        return;

    if (T->left != NULL)
        delete(T->left);
    
    if (T->right != NULL)
        delete(T->right);

    free(T);
}

int main() {
    struct Node * T = (struct Node *)calloc(sizeof(struct Node), 1);
    T->k = -1;

    int n; scanf("%d", &n);
    unsigned int s = 0, x, count = 0;

    for (int i = 0; i < n; ++i) {
        scanf("%d", &x);
        s ^= x;
        if (lookup(T, s) == NULL)
            insert(T, s, 0);

        count += (s == 0);
        count += (lookup(T, s)->v);
        lookup(T, s)->v += 1;
    }

    printf("%u\n", count);

    delete(T);

    return 0;
}