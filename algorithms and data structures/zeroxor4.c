#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define M 256

struct node {
    int key;
    int value;
    struct node *next;
};

struct node *get(struct node *n, int key) {
    while (n != NULL) {
        if (n->key == key) {
            return n;
        }
        n = n->next;
    }
    return NULL;
}

struct node *set(struct node *n, int key) {
    struct node *temp = malloc(sizeof(struct node));
    temp->key = key;
    temp->value = 0;
    temp->next = n->next;
    n->next = temp;
    return temp;
}

int main() {
    int m = M;
    struct node *arr[m];
    for (int i = 0; i < m; i++) {
        arr[i] = malloc(sizeof(struct node));
        arr[i]->next = NULL;
        arr[i]->key = -1;
    }

    int n, x;
    scanf("%d", &n);

    long long count = 0, s = 0;

    for (int i = 0; i < n; i++) {
        scanf("%d", &x);
        s ^= x;

        if (s == 0) {
            count++;
        }

        struct node * temp = get(arr[(s % m + m) % m], s);
        if (temp == NULL) {
            temp = set(arr[(s % m + m) % m], s);
        }

        count += temp->value;
        temp->value++;
    }
    printf("%lld\n", count);

    for (int i = 0; i < m; i++) {
        while (arr[i] != NULL) {
            struct node *temp = arr[i];
            arr[i] = temp->next;
            free(temp);
        }
    }
    return 0;
}