#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1000
#define ALPHABET_SIZE 256

typedef struct node {
    int prefix;
    char *label;
    struct node *parent;
    struct node *arcs[ALPHABET_SIZE];
} __attribute__((packed)) node_t;

typedef struct trie {
    node_t *root;
} __attribute__((packed)) trie_t;

typedef struct position {
    node_t *x;
    size_t i, d;
} __attribute__((packed)) position_t;

char *strjoin(const char *s1, const char *s2) {
    char *result = (char *)calloc(strlen(s1) + strlen(s2) + 1, sizeof(char));
    return strcat(strcpy(result, s1), s2);
}

char *substring(char *src, int s, int e) {
    char *buffer = (char *)calloc(e - s + 1, sizeof(char));
    return memcpy(buffer, src + s, e - s);
}

char *read_line(void) {
    int buffer_size = BUFFER_SIZE, pos = 0;
    char *buffer = (char *)calloc(buffer_size, sizeof(char)), symb;

    while (1) {
        symb = getchar();
        if (symb == EOF || symb == '\n') {
            buffer[pos] = '\0';
            break;
        }

        buffer[pos++] = symb;

        if (pos >= buffer_size) {
            buffer_size += BUFFER_SIZE;
            buffer = (char *)realloc(buffer, buffer_size);
        }
    }

    return buffer;
}

trie_t *create_trie(void) { return (trie_t *)calloc(1, sizeof(trie_t)); }

node_t *create_node(char *label) {
    node_t *n = (node_t *)calloc(1, sizeof(node_t));
    n->label = label;
    return n;
}

void free_node(node_t *n) {
    if (n == NULL)
        return;

    free(n->label);
    free(n);
}

void free_nodes(node_t *n) {
    if (n == NULL)
        return;

    for (int i = 0; i < ALPHABET_SIZE; ++i)
        free_nodes(n->arcs[i]);

    free_node(n);
}

void free_trie(trie_t *t) {
    free_nodes(t->root);
    free(t);
}

position_t descend(trie_t *t, char *k) {
    position_t pos = {.x = t->root, .i = 0, .d = 0};

    if (t->root == NULL)
        return pos;

    size_t length_k = strlen(k), length_l = 0;

    while (1) {
        char *l = pos.x->label;

        for (length_l = strlen(l);
             pos.d < length_k && pos.i < length_l && k[pos.d] == l[pos.i];
             ++pos.d, ++pos.i)
            ;

        if (pos.d == length_k || pos.i != length_l)
            break;

        node_t *y = pos.x->arcs[(int)k[pos.d]];

        if (y == NULL)
            break;

        pos = (position_t){.i = 0, .x = y, .d = pos.d};
    }

    return pos;
}

void update_prefix(trie_t *t, node_t *n, int v) {
    for (; n != NULL; n = n->parent)
        n->prefix += v;
}

void link(trie_t *t, node_t *parent, node_t *child) {
    child->parent = parent;

    if (parent == NULL)
        t->root = child;
    else
        parent->arcs[(int)child->label[0]] = child;
}

void split(trie_t *t, position_t *pos) {
    char *l = pos->x->label;

    if (pos->i == 0 && pos->x->parent != NULL) {
        pos->x = pos->x->parent;
        pos->i = strlen(pos->x->label);
        return;
    } else if (pos->i < strlen(l)) {
        node_t *y = create_node(substring(l, 0, pos->i));
        y->prefix = pos->x->prefix;

        pos->x->label = substring(l, pos->i, strlen(l));

        link(t, pos->x->parent, y);
        link(t, y, pos->x);

        pos->x = y;

        free(l);
    }
}

void insert(trie_t *t, char *k) {
    if (t->root == NULL) {
        t->root = create_node(substring(k, 0, strlen(k)));
        t->root->prefix = 1;
        return;
    }

    position_t pos = descend(t, k);
    split(t, &pos);

    if (pos.d == strlen(k))
        return update_prefix(t, pos.x, !pos.x->prefix);

    node_t *x = create_node(substring(k, pos.d, strlen(k)));

    link(t, pos.x, x);
    update_prefix(t, x, +1);
}

void simplify(trie_t *t, node_t *x) {
    node_t *z = NULL;

    for (int i = 0; i < ALPHABET_SIZE; ++i) {
        if (x->arcs[i] != NULL) {
            if (z != NULL)
                return;

            z = x->arcs[i];
        }
    }

    if (z == NULL || x->prefix != z->prefix)
        return;

    char *buffer = strjoin(x->label, z->label);
    free(z->label);
    z->label = buffer;

    link(t, x->parent, z);

    free_node(x);
}

void delete(trie_t *t, char *k) {
    position_t pos = descend(t, k);

    if (pos.d != strlen(k))
        return;

    node_t *x = pos.x;
    char l = x->label[0];
    node_t *y = x->parent;

    update_prefix(t, x, -1);

    if (x->prefix > 0)
        return simplify(t, x);

    free_nodes(x);

    if (y == NULL) {
        t->root = NULL;
        return;
    }

    y->arcs[(int)l] = NULL;
    simplify(t, y);
}

int prefix(trie_t *t, char *k) {
    position_t pos = descend(t, k);
    return pos.x == NULL || pos.d != strlen(k) ? 0 : pos.x->prefix;
}

int main(void) {
    trie_t *t = create_trie();

    char cmd[7], *k;
    while (1) {
        scanf("%s", cmd);

        if (!strcmp(cmd, "END"))
            break;

        getchar();
        k = read_line();

        if (!strcmp(cmd, "INSERT"))
            insert(t, k);

        if (!strcmp(cmd, "DELETE"))
            delete (t, k);

        if (!strcmp(cmd, "PREFIX"))
            printf("%d ", prefix(t, k));

        free(k);
    }
    printf("\n");

    free_trie(t);

    return 0;
}
