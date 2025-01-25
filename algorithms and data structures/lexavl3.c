#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <ctype.h>
#include <string.h>
void* safe_malloc(size_t size) {
    if (size == 0) {
        return NULL;
    }

    void* p = malloc(size);
    assert(p != NULL);

    return p;
}
void* safe_calloc(size_t nmemb, size_t size) {
    if (nmemb == 0 || size == 0) {
        return NULL;
    }
    assert(!(SIZE_MAX / nmemb <= size));
    void* p = calloc(nmemb, size);
    assert(p != NULL);
    return p;
}

void safe_free(void* pointer) {
    free(pointer);
}

int max(int a, int b) {
    return a > b ? a : b;
}

typedef char* avl_key;
typedef int avl_value;

typedef int(*avl_compare)(const char*, const char*);

struct AVL {
    avl_key key;
    avl_value value;
    int balance;
    struct AVL *left, *right, *parent;
};

struct AVL* create(avl_key key, avl_value value) {
    struct AVL* tree = safe_malloc(sizeof(struct AVL));

    tree->left = tree->right = tree->parent = NULL;
    tree->value = value;
    tree->balance = 0;

    const size_t size = strlen(key);
    tree->key = safe_calloc(size+1, sizeof(char));
    memcpy(tree->key, key, size);

    return tree;
}

void delete(struct AVL* tree) {
    if (tree == NULL) {
        return;
    }

    delete(tree->left);
    delete(tree->right);
    safe_free(tree->key);
    safe_free(tree);
}

struct AVL* search(struct AVL* tree, avl_key key, avl_compare compare) {
    struct AVL* node = tree;

    while (node != NULL) {
        const int status = compare(key, node->key);
        
        if (status < 0) {
            node = node->left;
        } else if (status > 0) {
            node = node->right;
        } else {
            return node;
        }
    }

    return NULL;
}

void rotate(struct AVL** tree, struct AVL* node, int direction) {
    struct AVL* node1, *node2;

    if (direction == 1) {
        node1 = node->right, node2 = node1->left;
    } else {
        node1 = node->left, node2 = node1->right;
    }

    if (node == *tree) {
        *tree = node1;

        if (node1 != NULL) {
            node1->parent = NULL;
        }
    } else {
        struct AVL* parent = node->parent;

        if (node1 != NULL) {
            node1->parent = parent;
        }

        if (parent->left == node) {
            parent->left = node1;
        } else {
            parent->right = node1;
        }
    }

    if (node2 != NULL) {
        node2->parent = node;
    }

    if (direction == 1) {
        node->right = node2, node1->left = node;
    } else {
        node->left = node2, node1->right= node;
    }

    node->parent = node1;

    node->balance -= direction;
    if ((direction == 1 && node1->balance > 0) || \
        (direction == -1 && node1->balance < 0)) {
        node->balance -= node1->balance;
    }

    node1->balance -= direction;
    if ((direction == 1 && node->balance < 0) || \
        (direction == -1 && node->balance > 0)) {
        node1->balance += node->balance;
    }
}

struct AVL* insert(struct AVL* tree, avl_key key, avl_value value, avl_compare compare) {
    struct AVL* new = create(key, value);

    if (tree == NULL) {
        return new;
    }

    struct AVL* current = tree, *last = NULL;

    while (current != NULL) {
        last = current;

        const int status = compare(key, current->key);

        if (status < 0) {
            current = current->left;
        } else if (status > 0) {
            current = current->right;
        }
    }

    if (compare(key, last->key) < 0) {
        last->left = new;
    } else {
        last->right = new;
    }

    new->parent = last;

    last = new;
    while (1) {
        current = last->parent;

        if (current == NULL) {
            break;
        }

        int direction = (current->left == last) ? -1 : 1;
        current->balance += direction;

        if (current->balance == 0) {
            break;
        }

        if (current->balance == -2) {
            if (last->balance == 1) {
                rotate(&tree, last, 1);
            }
            rotate(&tree, current, -1);
            break;
        } else if (current->balance == 2) {
            if (last->balance == -1) {
                rotate(&tree, last, -1);
            }
            rotate(&tree, current, 1);
            break;
        }

        last = current;
    }

    return tree;
}

int main() {
    int n;
    scanf("%d\n", &n);

    char* s = safe_calloc(n+1, sizeof(char));
    fgets(s, n+1, stdin);

    int punct[256] = {0};

    punct['+'] = 0, punct['-'] = 1;
    punct['*'] = 2, punct['/'] = 3;
    punct['('] = 4, punct[')'] = 5;

    char* ident = safe_malloc(n+1);
    int count = 0;

    struct AVL* tree = NULL;

    for (int i = 0; i < n; ++i) {
        if (ispunct(s[i])) {
            printf("SPEC %d\n", punct[s[i]]);
            continue;
        }

        if (isspace(s[i]) || !s[i]) {
            continue;
        }

        if (isdigit(s[i])) {
            printf("CONST %c", s[i]);
            for (; i+1 < n && isdigit(s[i+1]); i++) {
                printf("%c", s[i+1]);
            }
            putchar('\n');
            continue;
        }

        int size = 1;
        ident[0] = s[i];

        for (; i+1 < n && isalnum(s[i+1]); i++, size++) {
            ident[size] = s[i+1];
        }
        ident[size] = '\0';

        struct AVL* value = search(tree, ident, strcmp);

        if (value == NULL) {
            printf("IDENT %d\n", count);
            tree = insert(tree, ident, count, strcmp);
            ++count;
        } else {
            printf("IDENT %d\n", value->value);
        }
    }

    delete(tree);
    safe_free(ident);
    safe_free(s);

    return 0;
}

