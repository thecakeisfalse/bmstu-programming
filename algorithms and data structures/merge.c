#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
// #include <assert.h>
#include <stdint.h>

#define swap(a, b) { struct Item *temp = a; a = b; b = temp; }

struct Item {
    int index, key, value;
};

struct PriorityQueue {
    int capacity;
    int count;
    struct Item ** heap;
};

struct Item * create_item(int key, int value) {
    struct Item * item = (struct Item *)calloc(1, sizeof(struct Item));

    item->key = key;
    item->value = value;
    item->index = -1;

    return item;
}

void heapify(struct Item ** a, int n, size_t i) {
    while (1) {
        size_t l = 2*i+1, r = 2*i+2, j = i;

        if (l < n && a[i]->value > a[l]->value) i = l;
        if (r < n && a[i]->value > a[r]->value) i = r;
        if (i == j) break;

        swap(a[i], a[j]);
        a[i]->index = i;
        a[j]->index = j;
    }
}

struct PriorityQueue * init_priority_queue(int n) {
    struct PriorityQueue * queue = (struct PriorityQueue *)calloc(1, sizeof(struct PriorityQueue));

    queue->count = 0;
    queue->capacity = n;
    queue->heap = (struct Item **)calloc(queue->capacity, sizeof(struct Item *));

    return queue;
}

struct Item * maximum(struct PriorityQueue * queue) {
    //assert(queue->count > 0);
    return queue->heap[0];
}

bool empty(struct PriorityQueue * queue) {
    return queue->count == 0;
}

void insert(struct PriorityQueue * queue, struct Item * item) {
    //assert(queue->count != queue->capacity);
    
    int i = queue->count;
    queue->count = i+1;
    queue->heap[i] = item;

    for (; i > 0 && queue->heap[(i-1) / 2]->value > queue->heap[i]->value; i = (i-1) / 2) {
        swap(queue->heap[(i-1) / 2], queue->heap[i]);
        queue->heap[i]->index = i;
    }

    queue->heap[i]->index = i;
}

struct Item * extract_min(struct PriorityQueue * queue) {
    //assert(!empty(queue));

    struct Item * item = queue->heap[0];
    --queue->count;

    if (queue->count) {
        queue->heap[0] = queue->heap[queue->count];
        queue->heap[0]->index = 0;
        heapify(queue->heap, queue->count, 0);
    }

    return item;
}

void merge(int k, int l[], int * a[], int * result) {
    int n = 0;
    for (int i = 0; i < k; ++i)
        n += l[i];

    struct PriorityQueue * queue = init_priority_queue(n);
    int * usage = (int *)calloc(k, sizeof(int));
    for (int i = 0; i < k; ++i)
        insert(queue, create_item(i, a[i][usage[i]++]));

    for (int i = 0; i < n; ++i) {
        struct Item * item = extract_min(queue);
        result[i] = item->value;

        if (l[item->key] > usage[item->key])
            insert(queue, create_item(item->key, a[item->key][usage[item->key]++]));

        free(item);
    }

    free(queue->heap);
    free(queue);
    free(usage);
}

int main(void) {
    int k; scanf("%d", &k);
    int l[k];
    for (int i = 0; i < k; ++i)
        scanf("%d", &l[i]);
    
    int ** a = (int **)calloc(k, sizeof(int *)), n = 0;
    for (int i = 0; i < k; ++i) {
        a[i] = (int *)calloc(l[i], sizeof(int));
        for (int j = 0; j < l[i]; ++j)
            scanf("%d", &a[i][j]);
        n += l[i];
    }

    int * result = (int *)calloc(n, sizeof(int));

    merge(k, l, a, result);

    for (int i = 0; i < n; ++i)
        printf("%d ", result[i]);
    printf("\n");

    for (int i = 0; i < k; ++i)
        free(a[i]);
    free(a);

    free(result);

    return 0;
}