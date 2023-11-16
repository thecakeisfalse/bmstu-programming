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
    int *list;
    int *data;
    int count;
};

struct PriorityQueue *InitPriorityQueue(int size) {
    struct PriorityQueue *q = malloc(sizeof(struct PriorityQueue));
    q->count = 0;
    q->list = malloc(size * sizeof(int));
    q->data = malloc(size * sizeof(int));
    for (int i = 0; i < size; i++)
        q->list[i] = q->data[i] = 0;
    return q;
}

void Insert(struct PriorityQueue *q, int nlist, int value) {
    int i = q->count;
    q->list[i] = nlist;
    q->data[i] = value;

    while (i > 0 && q->data[i] < q->data[(i-1)/2]) {
        SWAP(q->list[i], q->list[(i-1)/2]);
        SWAP(q->data[i], q->data[(i-1)/2]);
        i = (i - 1) / 2;
    }

    q->count += 1;
}

void Heapify(struct PriorityQueue *q, int i, int n) {
    for (;;) {
        int l = 2 * i + 1, r = l + 1, j = i;

        if (l < n && q->data[i] > q->data[l])
            i = l;

        if (r < n && q->data[i] > q->data[r])
            i = r;

        if (i == j)
            break;

        SWAP(q->data[i], q->data[j]);
        SWAP(q->list[i], q->list[j]);
    }
}

int ExtractMin(struct PriorityQueue *q, int *nlist) {
    int ptr = q->data[0];
    *nlist = q->list[0];
    q->count -= 1;

    if (q->count > 0) {
        q->data[0] = q->data[q->count];
        q->list[0] = q->list[q->count];
        Heapify(q, 0, q->count);
    }

    return ptr;
}

int main(void) {
    int k = 0;
    scanf("%d", &k);

    struct PriorityQueue *q = InitPriorityQueue(k);

    int l[k];
    for (int i = 0; i < k; i++)
        scanf("%d\n", &l[i]);

    int *a[k], offset[k];
    int n = 0;
    for (int i = 0; i < k; i++) {
        a[i] = calloc(l[i], sizeof(int));
        offset[i] = 1;

        for (int j = 0; j < l[i]; j++)
            scanf("%d", &a[i][j]);

        Insert(q, i, a[i][0]);
        n += l[i];
    }

    int nlist = 0;
    for (int i = 0; i < n; i++) {
        int value = ExtractMin(q, &nlist);

        printf("%d\n", value);

        if (offset[nlist] < l[nlist]) {
            Insert(q, nlist, a[nlist][offset[nlist]]);
            offset[nlist] += 1;
        }
    }

    for (int i = 0; i < k; i++)
        free(a[i]);

    free(q->data);
    free(q->list);
    free(q);

    return 0;
}