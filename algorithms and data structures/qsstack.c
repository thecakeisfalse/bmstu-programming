#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Task {
    int low, high;
};

struct Stack {
    int capacity;
    int top;
    struct Task **data;
};

struct Stack *new_stack() {
    struct Stack *stack = (struct Stack *)calloc(1, sizeof(struct Stack));

    stack->capacity = 1;
    stack->top = 0;
    stack->data =
        (struct Task **)calloc(stack->capacity, sizeof(struct Task *));

    return stack;
}

void stack_push(struct Stack *stack, struct Task *task) {
    stack->data[stack->top++] = task;

    if (stack->top == stack->capacity) {
        stack->capacity <<= 1;
        stack->data =
            realloc(stack->data, stack->capacity * sizeof(struct Task));
        assert(stack->data != NULL);
    }
}

struct Task *stack_pop(struct Stack *stack) {
    assert(stack->top > 0);
    return stack->data[--stack->top];
}

void delete_stack(struct Stack *stack) {
    for (int i = 0; i < stack->top; ++i)
        free(stack->data[i]);

    free(stack->data);
    free(stack);
}

struct Task *create_task(int low, int high) {
    struct Task *task = (struct Task *)calloc(1, sizeof(struct Task));

    task->low = low;
    task->high = high;

    return task;
}

void swap(int *a, int *b) {
    int t = *a;
    *a = *b;
    *b = t;
}

int partition(int l, int r, int *a) {
    int pivot = a[r], q = l;

    for (int i = l; i < r; ++i)
        if (a[i] < pivot)
            swap(&a[i], &a[q++]);

    swap(&a[q], &a[r]);

    return q;
}

void quicksort(int n, int *a) {
    struct Stack *stack = new_stack();
    struct Task *task;
    stack_push(stack, create_task(0, n - 1));

    while (stack->top != 0) {
        task = stack_pop(stack);

        int q = partition(task->low, task->high, a);

        if (task->low < q - 1)
            stack_push(stack, create_task(task->low, q - 1));

        if (q + 1 < task->high)
            stack_push(stack, create_task(q + 1, task->high));

        free(task);
    }

    delete_stack(stack);
}

int main(void) {
    int n;
    scanf("%d", &n);
    int *a = (int *)calloc(n, sizeof(int));
    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i]);

    quicksort(n, a);

    for (int i = 0; i < n; ++i)
        printf("%d ", a[i]);
    printf("\n");

    free(a);

    return 0;
}