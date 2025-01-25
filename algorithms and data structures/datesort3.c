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

struct Date {
    int Day, Month, Year;
};

typedef struct Date sort_type;

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

int get_date_day(const struct Date* date) {
    return date->Day;
}

int get_date_month(const struct Date* date) {
    return date->Month;
}

int get_date_year(const struct Date* date) {
    return date->Year;
}

int main() {
    int n;
    scanf("%d", &n);

    struct Date* d = safe_calloc(n, sizeof(struct Date));
    for (int i = 0; i < n; i++) {
        scanf("%04d %02d %02d", &d[i].Year, &d[i].Month, &d[i].Day);
    }

    radix_sort(d, n, 1, 31, get_date_day);
    radix_sort(d, n, 1, 12, get_date_month);
    radix_sort(d, n, 1970, 2030, get_date_year);

    for (int i = 0; i < n; i++) {
        printf("%04d %02d %02d\n", d[i].Year, d[i].Month, d[i].Day);
    }

    free(d);

    return 0;
}
