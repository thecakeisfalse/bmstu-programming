#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

struct List{
    unsigned int index;
    int value;
    struct List *next;
};

struct Disparray{
    struct List **table;
    int m;
};

struct Disparray *InitDisparray(int m){
    struct Disparray *new_disparray= malloc(sizeof(struct Disparray));
    new_disparray->m = m;
    new_disparray->table = calloc(m, sizeof(struct List *));
    return new_disparray;
}

struct List *insert(struct List *list, unsigned int index, int value) {
    struct List *new_list = malloc(sizeof(struct List));
    new_list->next = list;
    new_list->value = value;
    new_list->index = index;
    return new_list;
}

struct List *Search(struct List *list, unsigned int index){
    while (list != NULL && list->index != index)
        list = list->next;
    return list;
}

void End(struct Disparray *disparray){
    for(int i = 0; i < disparray->m; i++){
        struct List *list = disparray->table[i%disparray->m];
        while (list!= NULL){
            struct List *next = list->next;
            free(list);
            list = next;
        }
    }
    free(disparray->table);
    free(disparray);
}

void Assign(struct Disparray *disparray, unsigned int key, int value) {
    struct List *element = Search(disparray->table[key%disparray->m], key);
    if (element == NULL){
        disparray->table[key%disparray->m] = insert(disparray->table[key%disparray->m], key, value);
    } else{
        element->value = value;
    }
}

int At(struct Disparray *disparray, unsigned int key) {
    struct List *element = Search(disparray->table[key%disparray->m], key);
    if (element == NULL) {
        return 0;
    } else {
        return element->value;
    }
}

int main() {
    int m = 256;
    struct Disparray *disparray = InitDisparray(m);

    int n;
    scanf("%d", &n);

    unsigned int prefixSum = 0, result = 0;
    for (int i = 0; i < n; i++) {
        int value;
        scanf("%d", &value);
        prefixSum ^= value;

        if (prefixSum == 0) {
            result++;
        }

        int lastPrefixSum = At(disparray, prefixSum);
        result += lastPrefixSum;
        Assign(disparray, prefixSum, lastPrefixSum+1);
    }

    printf("%u\n", result);

    End(disparray);

    return 0;
}