#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
// #include <assert.h>
#include <stdint.h>
#include <math.h>

#define swap(a, b) { struct Item *temp = a; a = b; b = temp; }

struct Item {
    int index, key;
    int end;
};

struct PriorityQueue {
    int capacity;
    int count;
    struct Item ** heap;
};

struct Item * create_item(int key, int end) {
    struct Item * item = (struct Item *)calloc(1, sizeof(struct Item));

    item->key = key;
    item->end = end;
    item->index = -1;

    return item;
}

void heapify(struct Item ** a, int n, size_t i) {
    while (1) {
        size_t l = 2*i+1, r = 2*i+2, j = i;

        if (l < n && a[i]->end > a[l]->end) i = l;
        if (r < n && a[i]->end > a[r]->end) i = r;
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

    for (; i > 0 && queue->heap[(i-1) / 2]->end > queue->heap[i]->end; i = (i-1) / 2) {
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
        result[i] = item->end;

        if (l[item->key] > usage[item->key])
            insert(queue, create_item(item->key, a[item->key][usage[item->key]++]));

        free(item);
    }

    free(queue->heap);
    free(queue);
    free(usage);
}

int main(void) {
    int n, m; scanf("%d%d", &n, &m);
    struct PriorityQueue * queue = init_priority_queue(n);
    int answer = 0;

    for (int i = 0, j = 0; i < m; ++i) {
        int t1, t2;
        scanf("%d%d", &t1, &t2);

        if (queue->count < n) {
            insert(queue, create_item(queue->count, t1 + t2));
            answer = fmax(answer, t1 + t2);
        } else {
            struct Item * item = extract_min(queue);
            int end = fmax(item->end, t1) + t2;
            insert(queue, create_item(item->key, end));
            answer = fmax(answer, end);
            free(item);
        }
    }


    for (int i = 0; i < queue->count; ++i) {
        answer = fmax(answer, queue->heap[i]->end);
        free(queue->heap[i]);
    }

    printf("%d\n", answer);

    free(queue->heap);
    free(queue);

    return 0;
}