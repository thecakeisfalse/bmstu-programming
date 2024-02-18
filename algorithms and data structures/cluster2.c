#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SWAP(a, b) \
    {              \
        int c = a; \
        a = b;     \
        b = c;     \
    }

struct PriorityQueue {
    int *heap;
    int count;
};

struct PriorityQueue *InitPriorityQueue(int size) {
    struct PriorityQueue *q = malloc(sizeof(struct PriorityQueue));
    q->count = 0;
    q->heap = malloc(size * sizeof(int));
    for (int i = 0; i < size; i++)
        q->heap[i] = 0;
    return q;
}

void Insert(struct PriorityQueue *q, int ptr) {
    int i = q->count;
    q->heap[i] = ptr;

    while (i > 0 && q->heap[i] < q->heap[(i-1)/2]) {
        SWAP(q->heap[i], q->heap[(i-1)/2]);
        i = (i - 1) / 2;
    }

    q->count += 1;
}

void Heapify(struct PriorityQueue *q, int i, int n) {
    for (;;) {
        int l = 2 * i + 1, r = l + 1, j = i;

        if (l < n && q->heap[i] > q->heap[l])
            i = l;

        if (r < n && q->heap[i] > q->heap[r])
            i = r;

        if (i == j)
            break;

        SWAP(q->heap[i], q->heap[j]);
    }
}

int ExtractMin(struct PriorityQueue *q) {
    int ptr = q->heap[0];
    q->count -= 1;

    if (q->count > 0) {
        q->heap[0] = q->heap[q->count];
        Heapify(q, 0, q->count);
    }

    return ptr;
}

int main(void) {
    int n, m;
    scanf("%d%d", &n, &m);

    struct PriorityQueue *q = InitPriorityQueue(n);

    for (int i = 0; i < m; i++) {
        int t1, t2;
        scanf("%d %d", &t1, &t2);
        int end = t1 + t2;
        
        if (!(q->count < n))
            end = fmax(t1, ExtractMin(q)) + t2;

        Insert(q, end);
    }

    int result = 0;
    while (q->count > 0)
        result = ExtractMin(q);

    printf("%d\n", result);

    free(q->heap);
    free(q);

    return 0;
}