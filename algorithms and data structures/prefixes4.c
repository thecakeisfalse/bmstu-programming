#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

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

/* Driver code */

vector* prefix_function(char* s, size_t length) {
    vector* prefix_table = vector_new();
    vector_ctr(prefix_table);

    vector_push_back(prefix_table, 0);

    for (size_t i = 1; i < length; i++) {
        int new_prefix = at(prefix_table, i-1);

        while (new_prefix > 0 && s[i] != s[new_prefix]) {
            new_prefix = at(prefix_table, new_prefix-1);
        }

        if (s[i] == s[new_prefix]) {
            new_prefix++;
        }

        vector_push_back(prefix_table, new_prefix);
    }

    return prefix_table;
}

vector* periodic_prefixes(vector* prefix_table, size_t length) {
    vector* periods = vector_new();
    vector_ctr(periods);

    vector_push_back(periods, 0);

    for (size_t i = 1; i <= length; i++) {
        const size_t prefix_length = i;
        const size_t possible_period = i - at(prefix_table, i-1);

        if (prefix_length % possible_period == 0) {
            vector_push_back(periods, prefix_length / possible_period);
        } else {
            vector_push_back(periods, 0);
        }
    }

    return periods;
}

int main(int argc, char* argv[]) {
    char* s = argv[1];
    const size_t length = strlen(s);

    vector* prefix_table = prefix_function(s, length);
    vector* periods_table = periodic_prefixes(prefix_table, length);

    for (size_t i = 1; i <= length; i++) {
        const int value = at(periods_table, i);
        if (value > 1) {
            printf("%d %d\n", i, value);
        }
    }

    vector_dtr(prefix_table);
    vector_dtr(periods_table);

    return 0;
}
