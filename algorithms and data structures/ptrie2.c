// https://ru.hexlet.io/courses/algorithms-trees/lessons/prefix/theory_unit (идея)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

struct node {
    struct node *arcs[26];
    int count;
    int leaf;
};

struct node *new_trie() {
    struct node *t = calloc(1, sizeof(struct node));
    t->count = 0;
    t->leaf = 0;

    for (int i = 0; i < 26; i++)
        t->arcs[i] = NULL;
    
    return t;
}

void free_tries(struct node *t) {
    if (t == NULL)
        return;

    for (int i = 0; i < 26; i++)
        free_tries(t->arcs[i]);
    
    free(t);
}

struct node *search(struct node *t, char *k) {
    int i = 0;

    while (k[i] != '\n' && k[i] != '\0') {
        int v = k[i]-'a';

        if (!t->arcs[v] || t->arcs[v] == 0)
            return NULL;
        
        t = t->arcs[v];
        i += 1;
    }

    return t;
}

void insert(struct node *t, char *k) {
    struct node *temp = search(t, k);
    if (temp != NULL && temp->leaf == 1)
        return;

    t->count += 1;

    int i = 0;
    while (k[i] != '\n' && k[i] != '\0') {
        int v = k[i]-'a';

        if (!t->arcs[v])
            t->arcs[v] = new_trie();

        t = t->arcs[v];
        t->count += 1;
        i += 1;
    }
    t->leaf = 1;
}

void delete(struct node *t, char *k) {
    struct node *temp = search(t, k);

    if (temp == NULL || temp->count == 0)
        return;

    t->count -= 1;

    int i = 0;
    while (k[i] != '\n' && k[i] != '\0') {
        int v = k[i]-'a';

        if (!t->arcs[v])
            break;

        t = t->arcs[v];
        t->count -= 1;
        i += 1;
    }

    t->leaf = 0;
}

int prefix(struct node *t, char *k) {
    struct node *n = search(t, k);
    return n ? n->count : 0;
}

int main() {
    struct node *t = new_trie();

    char *k = malloc(100001);
    memset(k, 0, 100001);

    char cmd[7] = {0};

    while (strcmp(cmd, "END")) {
        scanf("%s", cmd);

        if (strcmp(cmd, "INSERT") == 0) {
            getchar();
            fgets(k, 100001, stdin);
            insert(t, k);
        } else if (strcmp(cmd, "DELETE") == 0) {
            getchar();
            fgets(k, 100001, stdin);
            delete(t, k);
        } else if (strcmp(cmd, "PREFIX") == 0) {
            getchar();
            fgets(k, 100001, stdin);
            printf("%d ", prefix(t, k));
        }

        memset(k, 0, 100001);
    }

    free_tries(t);
    free(k);

    return 0;
}
