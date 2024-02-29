#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iso646.h>

#define DIGIT(x) ('0' <= x and x <= '9')
#define LETTER(x) (('a' <= x and x <= 'z') or ('A' <= x and x <= 'Z'))

typedef struct node {
    char *key;
    int value, balance;
    struct node *left, *right;
    struct node *parent;
} node;

typedef struct btree {
    node *root;
} btree;

typedef struct stream {
    char cur;
} stream;

stream *create_stream() {
    stream *s = malloc(sizeof(stream));
    s->cur = 0;
    return s;
}

char peek_stream(stream *s) {
    if (s->cur == 0)
        s->cur = getchar();

    return s->cur;
}

char next_stream(stream *s) {
    char result = peek_stream(s);
    s->cur = 0;
    return result;
}

node *create_node(char *key, int value) {
    node *n = malloc(sizeof(node));
    n->parent = n->left = n->right = NULL;
    n->key = malloc(strlen(key)+1);
    memcpy(n->key, key, strlen(key)+1);
    n->value = value;
    n->balance = 0;
    return n;
}

btree *create_btree() {
    btree *t = malloc(sizeof(btree));
    t->root = NULL;
    return t;
}

node *search(btree *t, char *key) {
    node *x = t->root;

    while (x != NULL and strcmp(x->key, key) != 0) {
        if (strcmp(key, x->key) < 0) {
            x = x->left;
        } else {
            x = x->right;
        }
    }

    return x;
}

node *insert(btree *t, char *key, int value) {
    node *y = create_node(key, value);
    if (t->root == NULL) {
        t->root = y;
    } else {
        node *x = t->root;
        for (;;) {
            if (strcmp(key, x->key) < 0) {
                if (x->left == NULL) {
                    x->left = y, y->parent = x;
                    break;
                }
                x = x->left;
            } else {
                if (x->right == NULL) {
                    x->right = y, y->parent = x;
                    break;
                }
                x = x->right;
            }
        }
    }
    return y;
}

void replace_node(btree *t, node *x, node *y) {
    if (t->root == x) {
        t->root = y;
        if (y != NULL) {
            y->parent = NULL;
        }
    } else {
        node *p = x->parent;
        if (y != NULL) {
            y->parent = p;
        }
        if (p->left == x) {
            p->left = y;
        } else {
            p->right = y;
        }
    }
}

void rotate_left(btree *t, node *x) {
    node *y = x->right;

    if (y != NULL) {
        replace_node(t, x, y);
        node *b = y->left;

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
}

void rotate_right(btree *t, node *x) {
    node *y = x->left;

    if (y != NULL) {
        replace_node(t, x, y);
        node *b = y->right;

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
}

void insert_avl(btree *t, char *key, int value) {
    node *a = insert(t, key, value);
    for (;;) {
        node *x = a->parent;
        if (x == NULL) {
            break;
        }

        if (a == x->left) {
            x->balance--;
            if (x->balance == 0) {
                break;
            }

            if (x->balance == -2) {
                if (a->balance == 1) {
                    rotate_left(t, a);
                }
                rotate_right(t, x);
                break;
            }
        } else {
            x->balance++;
            if (x->balance == 0) {
                break;
            }

            if (x->balance == 2) {
                if (a->balance == -1) {
                    rotate_right(t, a);
                }
                rotate_left(t, x);
                break;
            }
        }
        a = x;
    }
}

void delete_nodes(node *x) {
    if (x != NULL) {
        delete_nodes(x->right);
        delete_nodes(x->left);
        free(x->key);
        free(x);
    }
}

int main() {
    int n;
    scanf("%d\n", &n);
    stream *s = create_stream();
    int counter = 0;
    btree *t = create_btree();

    for (;;) {
        char cur = peek_stream(s);
        if (cur == '\n' || cur == 0) {
            break;
        } else if (cur == '+') {
            printf("SPEC 0\n");
            next_stream(s);
        } else if (cur == '-') {
            printf("SPEC 1\n");
            next_stream(s);
        } else if (cur == '*') {
            printf("SPEC 2\n");
            next_stream(s);
        } else if (cur == '/') {
            printf("SPEC 3\n");
            next_stream(s);
        } else if (cur == '(') {
            printf("SPEC 4\n");
            next_stream(s);
        } else if (cur == ')') {
            printf("SPEC 5\n");
            next_stream(s);
        } else {
            if (DIGIT(cur)) {
                printf("CONST ");
                while (DIGIT(cur)) {
                    printf("%c", next_stream(s));
                    cur = peek_stream(s);
                }
                printf("\n");
            } else if (LETTER(cur)) {
                char word[1000] = {0};
                int i = 0;
                while (DIGIT(cur) or LETTER(cur)) {
                    word[i] = next_stream(s);
                    i++;
                    cur = peek_stream(s);
                }

                node *n = search(t, word);
                if (n == NULL) {
                    printf("IDENT %d\n", counter);
                    insert_avl(t, word, counter);
                    counter++;
                } else {
                    printf("IDENT %d\n", n->value);
                }
            } else {
                next_stream(s);
            }
        }
    }

    delete_nodes(t->root);
    free(t);
    free(s);

    return 0;
}