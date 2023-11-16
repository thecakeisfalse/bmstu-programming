#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INF __INT_MAX__

typedef struct list {
    int value, max;
    struct list *next;
} list_t;

typedef struct {
    int size;
    list_t *head;
} stack_t;

typedef struct {
    int size;
    stack_t *input, *output;
} queue_t;

stack_t *init_stack() {
    stack_t *s = malloc(sizeof(stack_t));
    s->head = NULL;
    s->size = 0;
    return s;
}

queue_t *init_queue() {
    queue_t *q = malloc(sizeof(queue_t));
    q->input = init_stack();
    q->output = init_stack();
    q->size = 0;
    return q;
}

list_t *create_list(int v) {
    list_t *list = malloc(sizeof(list_t));
    list->next = NULL;
    list->value = v;
    return list;
}

void push(stack_t *s, int v) {
    list_t *n = create_list(v);
    n->next = s->head;
    n->max = s->head ? fmax(s->head->max, v) : v;
    s->head = n;
    s->size += 1;
}

int pop(stack_t *s) {
    if (s->head == NULL)
        return -INF;

    int elem = s->head->value;

    list_t * temp = s->head->next;
    free(s->head);
    s->head = temp;

    s->size -= 1;

    return elem;
}

void enqueue(queue_t *q, int v) {
    push(q->input, v);
    q->size += 1;
}

int dequeue(queue_t *q) {
    if (q->output->size == 0)
        while (q->input->size > 0)
            push(q->output, pop(q->input));

    if (q->output->size > 0) {
        q->size -= 1;
        return pop(q->output);
    }

    return -INF;
}

int maximum(queue_t *q) {
    return fmax(
        q->input->size > 0 ? q->input->head->max : -INF,
        q->output->size > 0 ? q->output->head->max : -INF
    );
}

int empty_queue(queue_t *q) {
    return q->size == 0;
}

void free_stack(stack_t *s) {
    while (s->head != NULL) {
        list_t *temp = s->head->next;
        free(s->head);
        s->head = temp;
    }
    free(s);
}

void free_queue(queue_t *q) {
    free_stack(q->input);
    free_stack(q->output);
    free(q);
}

int main(void) {
    queue_t *q = init_queue();

    char s[6] = {0};

    while (1) {
        scanf("%s", s);

        if (!strcmp(s, "END"))
            break;

        if (!strcmp(s, "EMPTY")) {
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

    free_queue(q);

    return 0;
}