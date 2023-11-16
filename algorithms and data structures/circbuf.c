#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Queue {
    int head, tail;
    int count;
    int capacity;
    int *data;
};

struct Queue *init_queue(void) {
    struct Queue *queue = (struct Queue *)calloc(1, sizeof(struct Queue));
    queue->capacity = 4;
    queue->count = queue->tail = queue->head = 0;
    queue->data = (int *)calloc(queue->capacity, sizeof(int));
    return queue;
}

bool empty_queue(struct Queue *queue) { return queue->count == 0; }

void enqueue(struct Queue *queue, int value) {
    ++queue->count;

    if (queue->count == queue->capacity) {
        int *temp = (int *)calloc(2 * queue->capacity, sizeof(int));
        assert(temp != NULL);

        int i, j;
        for (i = queue->head, j = 0; i != queue->tail;
             ++j, i = (i + 1) % queue->capacity)
            temp[j] = queue->data[i];

        free(queue->data);

        queue->data = temp;
        queue->capacity <<= 1;
        queue->tail = j;
        queue->head = 0;
    }

    queue->data[queue->tail] = value;
    queue->tail = (queue->tail + 1) % queue->capacity;
}

int dequeue(struct Queue *queue) {
    assert(!empty_queue(queue));

    --queue->count;

    int v = queue->data[queue->head];
    queue->head = (queue->head + 1) % queue->capacity;

    return v;
}

void delete_queue(struct Queue *queue) {
    free(queue->data);
    free(queue);
}

int main(void) {
    struct Queue *queue = init_queue();

    char s[6] = {0};

    while (1) {
        scanf("%s", s);
        if (s[2] == 'D') {
            break;
        } else if (s[1] == 'M') {
            printf("%s\n", (empty_queue(queue) ? "true" : "false"));
        } else if (s[0] == 'E') {
            int v;
            scanf("%d", &v);
            enqueue(queue, v);
        } else if (s[0] == 'D') {
            printf("%d\n", dequeue(queue));
        }
    }

    delete_queue(queue);

    return 0;
}