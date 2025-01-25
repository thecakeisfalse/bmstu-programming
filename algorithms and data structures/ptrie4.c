#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
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

/* Trie */

#define ALPHABET_SIZE 26

struct trie {
    struct trie* next[ALPHABET_SIZE];
    bool end_of_line;
    int count_matching;
};

struct trie* new() {
    struct trie* trie = safe_malloc(sizeof(struct trie));

    for (int i = 0; i < ALPHABET_SIZE; i++) {
        trie->next[i] = NULL;
    }

    trie->end_of_line = false;
    trie->count_matching = 0;

    return trie;
}

struct trie* find(struct trie* trie, const char* word) {
    struct trie* node = trie;
    for (int i = 0; word[i] != 0; i++) {
        const int index = word[i]-'a';
        if (node->next[index] == NULL) {
            return NULL;
        }
        node = node->next[index];
    }
    return node->end_of_line ? node : NULL;
}

void insert(struct trie* trie, const char* word) {
    if (find(trie, word) != NULL) {
        return;
    }
    struct trie* node = trie;
    for (int i = 0; word[i] != 0; i++) {
        const int index = word[i]-'a';
        if (node->next[index] == NULL) {
            node->next[index] = new();
        }
        node->count_matching++;
        node = node->next[index];
    }
    node->end_of_line = 1;
    node->count_matching++;
}

void delete(struct trie* trie, const char* word) {
    if (find(trie, word) == NULL) {
        return;
    }
    struct trie* node = trie;
    for (int i = 0; word[i] != 0; i++) {
        const int index = word[i]-'a';
        node->count_matching--;
        node = node->next[index];
    }
    node->end_of_line = 0;
    node->count_matching--;
}

int count_prefix(struct trie* trie, const char* word) {
    struct trie* node = trie;
    for (int i = 0; word[i] != 0; i++) {
        const int index = word[i]-'a';
        if (node->next[index] == NULL) {
            return 0;
        }
        node = node->next[index];
    }
    return node->count_matching;
}

void delete_trie(struct trie* trie) {
    if (trie == NULL) {
        return;
    }

    for (int i = 0; i < ALPHABET_SIZE; i++) {
        delete_trie(trie->next[i]);
    }
    free(trie);
}

/* Driver */

int main() {
    struct trie* trie = new();

    char command[10] = {0};
    char* string = safe_calloc(100001, sizeof(char));

    for (;;) {
        scanf("%s", command);

        if (strcmp(command, "END") == 0) {
            break;
        }

        scanf("%s", string);

        if (strcmp(command, "INSERT") == 0) {
            insert(trie, string);
        } else if (strcmp(command, "DELETE") == 0) {
            delete(trie, string);
        } else if (strcmp(command, "PREFIX") == 0) {
            printf("%d ", count_prefix(trie, string));
        }
    }

    safe_free(string);
    delete_trie(trie);

    return 0;
}
