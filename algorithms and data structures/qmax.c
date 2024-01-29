#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INFTY __INT32_MAX__

struct Queue {
    int top1, top2;
    int capacity;
    int *data;
    int *maxdata;
} __attribute__((packed));

struct Queue *init_queue(void) {
    struct Queue *q = (struct Queue *)calloc(1, sizeof(struct Queue));
    assert(q != NULL);

    q->capacity = 4;
    q->top1 = 0;
    q->top2 = q->capacity - 1;
    q->data = (int *)calloc(q->capacity, sizeof(int));
    q->maxdata = (int *)calloc(q->capacity, sizeof(int));

    for (int i = 0; i < q->capacity; ++i) {
        q->maxdata[i] = -INFTY;
        q->data[i] = -INFTY;
    }

    return q;
}

bool empty_queue(struct Queue *q) {
    assert(q != NULL);

    return (q->top1 == 0) && (q->capacity - q->top2 == 1);
}

void pushr(struct Queue *q, int value) {
    assert(q != NULL);
    assert(q->top1 <= q->top2);

    q->maxdata[q->top2] =
        (q->capacity - q->top2 == 1 ? value
                                    : fmax(value, q->maxdata[q->top2 + 1]));
    q->data[q->top2] = value;
    --q->top2;
}

void pushl(struct Queue *q, int value) {
    assert(q != NULL);
    assert(q->top1 <= q->top2);

    if (q->top1 == q->top2) {
        q->capacity <<= 1;

        q->data = realloc(q->data, sizeof(int) * q->capacity);
        assert(q->data != NULL);

        q->maxdata = realloc(q->maxdata, sizeof(int) * q->capacity);
        assert(q->maxdata != NULL);

        int end = (q->capacity >> 1) - 1, st = q->top2;
        q->top2 = q->capacity - 1;

        for (; end > st; --end)
            pushr(q, q->data[end]);
    }

    q->maxdata[q->top1] =
        (q->top1 == 0 ? value : fmax(value, q->maxdata[q->top1 - 1]));
    q->data[q->top1] = value;

    ++q->top1;
}

int popl(struct Queue *q) {
    assert(q != NULL);
    assert(!(q->top1 == 0));

    return q->data[--q->top1];
}

int popr(struct Queue *q) {
    assert(q != NULL);
    assert(!(q->capacity - q->top2 == 1));

    return q->data[++q->top2];
}

void enqueue(struct Queue *q, int value) {
    assert(q != NULL);

    pushl(q, value);
}

int dequeue(struct Queue *q) {
    assert(q != NULL);
    assert(!empty_queue(q));

    if (q->capacity - q->top2 == 1) {
        while (q->top1 != 0) {
            pushr(q, popl(q));
        }
    }

    return popr(q);
}

int maximum(struct Queue *q) {
    return fmax(
        (q->top1 == 0 ? -INFTY : q->maxdata[q->top1 - 1]),
        (q->capacity - q->top2 == 1 ? -INFTY : q->maxdata[q->top2 + 1]));
}

void delete_queue(struct Queue *q) {
    free(q->data);
    free(q->maxdata);
    free(q);
}

int main(void) {
    struct Queue *q = init_queue();
    assert(q != NULL);

    char s[6] = {0};
    int i = 0;

    while (1) {
        scanf("%s", s);
        if (!strcmp(s, "END")) {
            break;
        } else if (!strcmp(s, "EMPTY")) {
            printf("%s\n", (empty_queue(q) ? "true" : "false"));
        } else if (!strcmp(s, "ENQ")) {
            int v;
            scanf("%d", &v);
            enqueue(q, v);
        } else if (!strcmp(s, "DEQ")) {
            printf("%d\n", dequeue(q));
        } else if (!strcmp(s, "MAX")) {
            printf("%d\n", maximum(q));
        }
    }

    delete_queue(q);

    return 0;
}