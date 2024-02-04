// https://neerc.ifmo.ru/wiki/index.php?title=Двоичная_куча

#include <stdio.h>
#include <stdlib.h>

void swap(int *a, int *b) {
    *a ^= *b;
    *b ^= *a;
    *a ^= *b;
}

int max(int a, int b) { return a > b ? a : b; }

typedef struct pqueue {
    int count;
    int *heap;
} pqueue_t;

pqueue_t *init_pqueue(int n) {
    pqueue_t *q = malloc(sizeof(pqueue_t));
    q->count = 0;
    q->heap = calloc(n, sizeof(int));
    return q;
}

void delete_pqueue(pqueue_t *q) {
    free(q->heap);
    free(q);
}

void siftDown(pqueue_t *q, int i) {
    while (2 * i + 1 < q->count) {
        int l = 2 * i + 1, r = 2 * i + 2, j = l;

        if (r < q->count && q->heap[r] < q->heap[l])
            j = r;

        if (q->heap[i] <= q->heap[j])
            break;

        swap(&q->heap[i], &q->heap[j]);
        i = j;
    }
}

void siftUp(pqueue_t *q, int i) {
    while (q->heap[i] < q->heap[(i - 1) / 2]) {
        swap(&q->heap[i], &q->heap[(i - 1) / 2]);
        i = (i - 1) / 2;
    }
}

int extract_min(pqueue_t *q) {
    int min = q->heap[0];
    q->heap[0] = q->heap[q->count - 1];
    q->count -= 1;
    siftDown(q, 0);
    return min;
}

void insert(pqueue_t *q, int key) {
    q->count += 1;
    q->heap[q->count - 1] = key;
    siftUp(q, q->count - 1);
}

int main(void) {
    int n, m;
    scanf("%d%d", &n, &m);
    pqueue_t *q = init_pqueue(n);
    int answer = 0;

    for (int i = 0; i < m; ++i) {
        int t1, t2;
        scanf("%d%d", &t1, &t2);
        int end = (q->count < n ? t1 : max(extract_min(q), t1)) + t2;
        insert(q, end);
        answer = max(answer, end);
    }

    for (int i = 0; i < q->count; ++i)
        answer = max(answer, q->heap[i]);

    printf("%d\n", answer);

    delete_pqueue(q);

    return 0;
}