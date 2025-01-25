#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

/* Utilities */

void* safe_malloc(size_t size) {
    if (size == 0) {
        return NULL;
    }

    void* p = malloc(size);
    assert(p != NULL);

    return p;
}

void* safe_calloc(size_t nmemb, size_t size) {
    if (nmemb == 0 || size == 0) {
        return NULL;
    }

    assert(!(SIZE_MAX / nmemb <= size));

    void* p = calloc(nmemb, size);
    assert(p != NULL);

    return p;
}

void safe_free(void* pointer) {
    free(pointer);
}

/* Vector */

const size_t DEFAULT_VECTOR_CAPACITY = 8;
const size_t VECTOR_GROWTH_FACTOR = 2;

typedef int vector_type;

struct vector {
    vector_type* elements;
    size_t capacity;
    size_t size;
};

typedef struct vector vector;

vector* vector_new() {
    return safe_malloc(sizeof(vector));
}

void vector_ctr(vector* vec) {
    assert(vec != NULL);

    vec->elements = safe_calloc(DEFAULT_VECTOR_CAPACITY+1, sizeof(vector_type));
    vec->capacity = DEFAULT_VECTOR_CAPACITY;
    vec->size = 0;
}

void vector_dtr(vector* vec) {
    assert(vec != NULL);

    safe_free(vec->elements);
    safe_free(vec);
}

size_t vector_size(vector* vec) {
    assert(vec != NULL);
    return vec->size;
}

size_t vector_capacity(vector* vec) {
    assert(vec != NULL);
    return vec->capacity;
}

bool vector_empty(vector* vec) {
    return vector_size(vec) == 0;
}

void vector_clear(vector* vec) {
    assert(vec != NULL);
    vec->size = 0;
}

void vector_push_back(vector* vec, const vector_type element) {
    assert(vec != NULL);
    assert(vec->elements != NULL);

    if (vec->size > 0 && vec->size == vec->capacity) {
        const size_t new_capacity = vec->capacity * VECTOR_GROWTH_FACTOR;

        vector_type* buffer = safe_calloc(new_capacity+1, sizeof(vector_type));

        memcpy(buffer, vec->elements, vec->capacity * sizeof(vector_type));
        safe_free(vec->elements);

        vec->capacity = new_capacity;
        vec->elements = buffer;
    }

    vec->elements[vec->size] = element;
    vec->size++;
}

vector_type at(vector* vec, const size_t index) {
    assert(vec != NULL);
    assert(vec->elements != NULL);
    assert(index < vec->size);

    return vec->elements[index];
}

vector_type* vector_begin(vector* vec) {
    assert(vec != NULL);
    assert(vec->elements != NULL);

    return &vec->elements[0];
}

vector_type* vector_end(vector* vec) {
    assert(vec != NULL);
    assert(vec->elements != NULL);

    return &vec->elements[vec->size];
}

#define foreach(it, vec) \
    for (vector_type* it = vector_begin(vec); it != vector_end(vec); it++)

/* Radix Sort and Driver */

union Int32 {
    int x;
    unsigned char bytes[4];
};

typedef union Int32 sort_type;

typedef int (*value_getter)(const sort_type*);

void radix_sort(sort_type * a, size_t n, int min_value, int max_value, value_getter get_value) {
    const int c = max_value - min_value + 1;

    vector* b[c];
    sort_type* buffer = safe_calloc(n, sizeof(sort_type));

    for (int i = 0; i < c; i++) {
        b[i] = vector_new();
        vector_ctr(b[i]);
    }

    for (size_t i = 0; i < n; i++) {
        const int value = get_value(&a[i]) - min_value;
        vector_push_back(b[value], i);
    }

    int k = 0;
    for (int i = 0; i < c; i++) {
        for (size_t j = 0; j < vector_size(b[i]); j++) {
            buffer[k++] = a[at(b[i], j)];
        }
        vector_dtr(b[i]);
    }

    memcpy(a, buffer, n*sizeof(sort_type));

    safe_free(buffer);
}

#define NEW_BYTE_GETTER(index)                   \
    int get_byte_##index(const union Int32* a) { \
        return a->bytes[index];                  \
    }

#define BYTE_GETTER(index) get_byte_##index

NEW_BYTE_GETTER(0);
NEW_BYTE_GETTER(1);
NEW_BYTE_GETTER(2);
NEW_BYTE_GETTER(3);

int main() {
    int n;
    scanf("%d", &n);

    union Int32* a = safe_calloc(n, sizeof(union Int32));

    for (int i = 0; i < n; i++) {
        scanf("%d", &a[i].x);
        // invert sign bit
        a[i].bytes[3] ^= (1 << 7);
    }

    radix_sort(a, n, 0, 255, BYTE_GETTER(0));
    radix_sort(a, n, 0, 255, BYTE_GETTER(1));
    radix_sort(a, n, 0, 255, BYTE_GETTER(2));
    radix_sort(a, n, 0, 255, BYTE_GETTER(3));

    for (int i = 0; i < n; i++) {
        a[i].bytes[3] ^= (1 << 7);
        printf("%d\n", a[i].x);
    }

    free(a);

    return 0;
}
