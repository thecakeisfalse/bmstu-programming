#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct queue {
    int l, r;
    int size;
    int capacity;
    int *data;
};

struct queue *InitQueue(void) {
    struct queue *q = malloc(sizeof(struct queue));
    q->capacity = 4;
    q->size = q->l = q->r = 0;
    q->data = malloc(q->capacity * sizeof(int));
    return q;
}

int QueueEmpty(struct queue *q) {
    return q->size == 0;
}

void Enqueue(struct queue *q, int x) {
    q->size += 1;

    if (q->size == q->capacity) {
        int *temp = (int *)calloc(2 * q->capacity, sizeof(int));

        int j = 0;
        for (;;) {
            int i = q->l + j;

            while (i >= q->capacity)
                i -= q->capacity;

            if (i == q->r)
                break;

            temp[j] = q->data[i];
            j += 1;
        }

        free(q->data);

        q->l = 0;
        q->r = j;
        q->data = temp;
        q->capacity *= 2;
    }

    q->data[q->r] = x;
    q->r += 1;

    if (q->r == q->capacity)
        q->r = 0;
}

int Dequeue(struct queue *q) {
    q->size -= 1;

    int x = q->data[q->l];
    q->l += 1;

    if (q->l == q->capacity)
        q->l = 0;

    return x;
}

int main(void) {
    struct queue *q = InitQueue();

    char s[6] = {0};

    for (;;) {
        scanf("%s", s);
        if (strcmp(s, "END") == 0) {
            break;
        } else if (strcmp(s, "EMPTY") == 0) {
            if (QueueEmpty(q))
                printf("true\n");
            else
                printf("false\n");
        } else if (strcmp(s, "ENQ") == 0) {
            int x;
            scanf("%d", &x);
            Enqueue(q, x);
        } else if (strcmp(s, "DEQ") == 0) {
            printf("%d\n", Dequeue(q));
        }
    } 

    free(q->data);
    free(q);

    return 0;
}