#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iso646.h>

#define M 27

typedef struct node {
    int key;
    char *value;
    struct node **next;
    int *span;
} node;

typedef struct {
    node *root;
} skiplist;

node *create_node(int key, char *value) {
    node *n = malloc(sizeof(node));
    n->key = key;

    if (value == NULL) {
        n->value = value;
    } else {
        n->value = malloc(strlen(value)+1);
        memcpy(n->value, value, strlen(value)+1);
    }

    n->next = malloc(M * sizeof(node *));
    for (int i = 0; i < M; i++) {
        n->next[i] = NULL;
    }
    
    n->span = malloc(M * sizeof(int));
    memset(n->span, 0, M * sizeof(int));

    return n;
}

skiplist *create_skiplist() {
    skiplist *sk = malloc(sizeof(skiplist));
    sk->root = create_node(-1111111111, NULL);
    return sk;
}

void delete_node(node *n) {
    free(n->next);
    free(n->span);
    free(n->value);
    free(n);
}

void delete_nodes(node *n) {
    if (n == NULL) {
        return;
    }

    delete_nodes(n->next[0]);
    delete_node(n);
}

void delete_skiplist(skiplist *list) {
    delete_nodes(list->root);
    free(list);
}

void skip(skiplist *list, int key, node **p) {
    node *x = list->root;
    for (int i = M-1; i >= 0; i--) {
        while (x->next[i] != NULL and x->next[i]->key < key) {
            x = x->next[i];
        }
        p[i] = x;
    }
}

int rank(skiplist *list, int key) {
    node *x = list->root;

    int sum = 0;
    for (int i = M-1; i >= 0; i--) {
        while (x->next[i] != NULL and x->next[i]->key < key) {
            sum += x->span[i];
            x = x->next[i];
        }
    }

    if (key == -1111111111)
        return -1;

    return sum;
}

node *lookup(skiplist *list, int key) {
    node *p[M];
    skip(list, key, p);
    return p[0]->next[0];
}

void insert(skiplist *list, int key, char *value) {
    node *p[M];
    skip(list, key, p);

    node *x = create_node(key, value);
    int r = rand() % M + 1;
    int current_rank = rank(list, p[0]->key);

    for (int i = 0; i < r; i++) {
        x->next[i] = p[i]->next[i];
        p[i]->next[i] = x;

        int new_rank = rank(list, p[i]->key);
        x->span[i] = p[i]->span[i] + new_rank - current_rank;
        p[i]->span[i] = current_rank - new_rank + 1;
    }

    for (int i = r; i < M; i++) {
        p[i]->span[i]++;
    }
}

void delete(skiplist *list, int key) {
    node *p[M];
    skip(list, key, p);

    node *x = p[0]->next[0];

    for (int i = 0; i < M; i++) {
        p[i]->span[i]--;
    }

    for (int i = 0; i < M and p[i]->next[i] == x; i++) {
        p[i]->next[i] = x->next[i];
        p[i]->span[i] += x->span[i];
    }

    delete_node(x);
}

int main(void) {
    skiplist *list = create_skiplist();

    char command[7];
    while (1) {
        scanf("%s", command);

        if (!strcmp(command, "END")) {
            break;
        } else if (!strcmp(command, "LOOKUP")) {
            int key;
            scanf("%d", &key);
            node *n = lookup(list, key);
            if (n != NULL and n->value != NULL) {
                printf("%s\n", n->value);
            }
        } else if (!strcmp(command, "INSERT")) {
            int key;
            char value[1000];
            scanf("%d %s", &key, value);
            insert(list, key, value);
        } else if (!strcmp(command, "DELETE")) {
            int key;
            scanf("%d", &key);
            delete (list, key);
        } else if (!strcmp(command, "RANK")) {
            int key;
            scanf("%d", &key);
            printf("%d\n", rank(list, key));
        }
    }

    delete_skiplist(list);

    return 0;
}