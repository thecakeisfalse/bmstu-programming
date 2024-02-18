#include <stdio.h>
#include <stdlib.h>

int max(int a, int b) {
    return a > b ? a : b;
}

typedef struct {
    int *list;
    int heapSize;
} BinaryHeap;

BinaryHeap *binaryHeapInit(int size) {
    BinaryHeap *binaryHeap = malloc(sizeof(BinaryHeap));
    binaryHeap->heapSize = 0;
    binaryHeap->list = malloc(size * sizeof(int));

    for (int i = 0; i < size; i++) {
        binaryHeap->list[i] = 0;
    }

    return binaryHeap;
}

void insert(BinaryHeap *binaryHeap, int elem) {
    binaryHeap->list[binaryHeap->heapSize] = elem;

    int i = binaryHeap->heapSize;
    int parent = (i - 1) / 2;

    while (i > 0 && binaryHeap->list[i] < binaryHeap->list[parent]) {
        int temp = binaryHeap->list[i];
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
            binaryHeap->list[leftChild] < binaryHeap->list[smallestChild]) {
            smallestChild = leftChild;
        }

        if (rightChild < binaryHeap->heapSize &&
            binaryHeap->list[rightChild] < binaryHeap->list[smallestChild]) {
            smallestChild = rightChild;
        }

        if (smallestChild == i) {
            break;
        }

        int temp = binaryHeap->list[i];
        binaryHeap->list[i] = binaryHeap->list[smallestChild];
        binaryHeap->list[smallestChild] = temp;

        i = smallestChild;
    }
}

int extractMin(BinaryHeap *binaryHeap) {
    int result = binaryHeap->list[0];
    binaryHeap->heapSize -= 1;

    if (binaryHeap->heapSize > 0) {
        binaryHeap->list[0] = binaryHeap->list[binaryHeap->heapSize];
        heapify(binaryHeap, 0);
    }

    return result;
}

void deleteBinaryHeap(BinaryHeap *binaryHeap) {
    free(binaryHeap->list);
    free(binaryHeap);
}

int main() {
    int n, m;
    scanf("%d %d", &n, &m);

    BinaryHeap *binaryHeap = binaryHeapInit(n);

    for (int i = 0; i < m; i++) {
        int start, workingTime;
        scanf("%d %d", &start, &workingTime);

        // Если есть хотя бы одно свободное ядро,
        //      процесс можно запускать с момента start.
        // Если же всё занято, то мы находим, когда
        //      заканчивается самый первый процесс из очереди
        //      и запускаем процесс с max(start, время последнего процесса).

        int finish = 0;

        if (binaryHeap->heapSize < n) {
            finish = start + workingTime;
        } else {
            finish = max(start, extractMin(binaryHeap)) + workingTime;
        }

        insert(binaryHeap, finish);
    }

    int result = 0;
    while (binaryHeap->heapSize > 0) {
        result = extractMin(binaryHeap);
    }

    printf("%d\n", result);

    deleteBinaryHeap(binaryHeap);

    return 0;
}