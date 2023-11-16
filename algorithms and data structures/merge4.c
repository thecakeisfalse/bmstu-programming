// https://habr.com/ru/articles/112222/
// https://www.scaler.com/topics/merge-k-sorted-arrays/

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int k;
    int i;
    int value;
} Elem;

typedef struct {
    Elem **list;
    int heapSize;
} BinaryHeap;

BinaryHeap *binaryHeapInit(int size) {
    BinaryHeap *binaryHeap = malloc(sizeof(BinaryHeap));
    binaryHeap->heapSize = 0;
    binaryHeap->list = malloc(size * sizeof(Elem *));

    for (int i = 0; i < size; i++) {
        binaryHeap->list[i] = NULL;
    }

    return binaryHeap;
}

Elem *newElem(int k, int i, int value) {
    Elem *elem = malloc(sizeof(Elem));
    elem->k = k;
    elem->i = i;
    elem->value = value;
    return elem;
}

void insert(BinaryHeap *binaryHeap, Elem *elem) {
    binaryHeap->list[binaryHeap->heapSize] = elem;

    int i = binaryHeap->heapSize;
    int parent = (i - 1) / 2;

    while (i > 0 && binaryHeap->list[i]->value < binaryHeap->list[parent]->value) {
        Elem *temp = binaryHeap->list[i];
        binaryHeap->list[i] = binaryHeap->list[parent];
        binaryHeap->list[parent] = temp;

        i = parent;
        parent = (i - 1) / 2;
    }

    binaryHeap->heapSize += 1;
}

void heapify(BinaryHeap *binaryHeap, int i) {
    int leftChild, rightChild, smallestChild;

    for (;;) {
        leftChild = 2 * i + 1;
        rightChild = leftChild + 1;
        smallestChild = i;

        if (leftChild < binaryHeap->heapSize &&
            binaryHeap->list[leftChild]->value < binaryHeap->list[smallestChild]->value) {
            smallestChild = leftChild;
        }

        if (rightChild < binaryHeap->heapSize &&
            binaryHeap->list[rightChild]->value < binaryHeap->list[smallestChild]->value) {
            smallestChild = rightChild;
        }

        if (smallestChild == i) {
            break;
        }

        Elem *temp = binaryHeap->list[i];
        binaryHeap->list[i] = binaryHeap->list[smallestChild];
        binaryHeap->list[smallestChild] = temp;

        i = smallestChild;
    }
}

Elem *extractMin(BinaryHeap *binaryHeap) {
    Elem *result = binaryHeap->list[0];
    binaryHeap->heapSize -= 1;

    if (binaryHeap->heapSize > 0) {
        binaryHeap->list[0] = binaryHeap->list[binaryHeap->heapSize];
        binaryHeap->list[binaryHeap->heapSize] = NULL;
        heapify(binaryHeap, 0);
    }

    return result;
}

void deleteBinaryHeap(BinaryHeap *binaryHeap) {
    for (int i = 0; i < binaryHeap->heapSize; i++) {
        free(binaryHeap->list[i]);
    }

    free(binaryHeap->list);
    free(binaryHeap);
}

int main() {
    int k;
    scanf("%d", &k);

    BinaryHeap *binaryHeap = binaryHeapInit(k);

    int sizes[k];
    for (int i = 0; i < k; i++) {
        scanf("%d\n", &sizes[i]);
    }

    int *arrays[k];
    for (int i = 0; i < k; i++) {
        arrays[i] = malloc(sizes[i] * sizeof(int));

        for (int j = 0; j < sizes[i]; j++) {
            scanf("%d", &arrays[i][j]);
        }

        insert(binaryHeap, newElem(i, 1, arrays[i][0]));
    }

    while (binaryHeap->heapSize != 0) {
        Elem *elem = extractMin(binaryHeap);
        printf("%d\n", elem->value);

        if (elem->i < sizes[elem->k]) {
            insert(binaryHeap, newElem(elem->k, elem->i + 1, arrays[elem->k][elem->i]));
        }

        free(elem);
    }

    for (int i = 0; i < k; i++)
        free(arrays[i]);

    deleteBinaryHeap(binaryHeap);

    return 0;
}