#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define INF 2147483647

#define NEW_STACK(s, cap)                      \
    {                                          \
        s = malloc(sizeof(struct stack));      \
        s->size = 0;                           \
        s->capacity = cap;                     \
        s->maxdata = calloc(cap, sizeof(int)); \
        s->data = calloc(cap, sizeof(int));    \
    }

#define DELETE_STACK(s)       \
    {                         \
        if (s != NULL)        \
        {                     \
            free(s->maxdata); \
            free(s->data);    \
            free(s);          \
            s = NULL;         \
        }                     \
    }

#define STACK_MAXIMUM(s) (s->size ? s->maxdata[s->size-1] : -INF)

struct stack {
    int size;
    int capacity;
    int *maxdata;
    int *data;
};

struct queue {
    struct stack *s1;
    struct stack *s2;
};

typedef void (*function_t)(struct queue *);

struct command {
    char *name;
    function_t func;
};

struct commands {
    int n;
    struct command **commands;
};

void push(struct stack *s, int value) {
    if (s->size == s->capacity) {
        s->capacity *= 2;
        s->data = realloc(s->data, sizeof(int) * s->capacity);
        s->maxdata = realloc(s->maxdata, sizeof(int) * s->capacity);
    }

    s->data[s->size] = value;

    if (s->size == 0)
        s->maxdata[s->size] = value;
    else
        s->maxdata[s->size] = fmax(value, s->maxdata[s->size-1]);

    s->size += 1;
}

int pop(struct stack *s) {
    if (s->size == 0)
        return -INF;
    
    return s->data[--s->size];
}

struct queue *InitQueue() {
    struct queue *q = malloc(sizeof(struct queue));
    NEW_STACK(q->s1, 4);
    NEW_STACK(q->s2, 4);
    return q;
}

void Enqueue(struct queue *q) {
    int value;
    scanf("%d", &value);
    push(q->s1, value);
}

void Dequeue(struct queue *q) {
    if (q->s2->size == 0)
        while (q->s1->size > 0)
            push(q->s2, pop(q->s1));

    if (q->s2->size > 0) {
        printf("%d\n", pop(q->s2));
        return;
    }

    printf("%d\n", -INF);
}

void Maximum(struct queue *q) {
    int max_s1 = STACK_MAXIMUM(q->s1);
    int max_s2 = STACK_MAXIMUM(q->s2);
    printf("%d\n", (int)fmax(max_s1, max_s2));
}

void QueueEmpty(struct queue *q) {
    if (q->s1->size + q->s2->size == 0)
        printf("true\n");
    else
        printf("false\n");
}

struct commands *InitCommands(void) {
    struct commands *cmds = malloc(sizeof(struct commands));
    cmds->commands = NULL;
    cmds->n = 0;
    return cmds;
}

void DeleteCommands(struct commands *c) {
    for (int i = 0; i < c->n; i++) {
        free(c->commands[i]);
    }
    free(c->commands);
    free(c);
}

void AddCommand(struct commands *c, char *name, function_t func) {
    c->n++;
    c->commands = realloc(c->commands, c->n * sizeof(struct command *));
    c->commands[c->n - 1] = malloc(sizeof(struct command));
    c->commands[c->n - 1]->name = name;
    c->commands[c->n - 1]->func = func;
}

int main(void) {
    struct queue *q = InitQueue();

    struct commands *commands = InitCommands();
    AddCommand(commands, "EMPTY", QueueEmpty);
    AddCommand(commands, "ENQ", Enqueue);
    AddCommand(commands, "DEQ", Dequeue);
    AddCommand(commands, "MAX", Maximum);

    char command[10] = {0};
    while (strcmp(command, "END")) {
        scanf("%s", command);
        for (int i = 0; i < commands->n; i++) {
            if (!strcmp(commands->commands[i]->name, command)) {
                commands->commands[i]->func(q);
            }
        }
    }

    DeleteCommands(commands);

    DELETE_STACK(q->s1);
    DELETE_STACK(q->s2);
    free(q);

    return 0;
}