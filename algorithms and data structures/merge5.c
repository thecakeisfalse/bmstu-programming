#include <stdio.h>
#include <stdlib.h>
#include <iso646.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

void swap(void *p1, void *p2, size_t size)
{
  char substitute[size];
  memmove(substitute, p1, size);
  memmove(p1, p2, size);
  memmove(p2, substitute, size);
}

typedef struct
{
  int* data;
  int head, tail;
} Queue;

Queue* newQueue(int size)
{
  Queue* queue = (Queue*)malloc(sizeof(Queue));
  queue->head = queue->tail = 0;
  queue->data = (int*)calloc(size, sizeof(int));
  return queue;
}

void deleteQueue(Queue* queue)
{
  free(queue->data);
  free(queue);
}

void QueueEnqueue(Queue* queue, int value)
{
  queue->data[queue->tail] = value;
  queue->tail++;
}

void QueueDequeue(Queue* queue)
{
  queue->head++;
}

int GetQueueFront(Queue* queue)
{
  return queue->data[queue->head];
}

bool IsQueueEmpty(Queue* queue)
{
  return queue->head >= queue->tail;
}

/*
 * @doc: https://neerc.ifmo.ru/wiki/index.php?title=Двоичная_куча
 * @code: https://ru.algorithmica.org/cs/basic-structures/heap/
 */

typedef struct
{
  Queue** data;
  int size;
} Heap;

Heap* newHeap(int size)
{
  Heap* heap = (Heap*)malloc(sizeof(Heap));
  heap->size = 0;
  heap->data = (Queue**)calloc(size, sizeof(Queue*));
  return heap;
}

void deleteHeap(Heap* heap)
{
  for (int i = 0; i < heap->size; i++)
  {
    deleteQueue(heap->data[i]);
  }
  free(heap->data);
  free(heap);
}

void SiftUp(Heap* heap, int v)
{
  while (v > 0 and GetQueueFront(heap->data[v]) < GetQueueFront(heap->data[(v-1)/2]))
  {
    swap(&heap->data[v], &heap->data[(v-1)/2], sizeof(Queue*));
    v = (v-1) / 2;
  }
}

void SiftDown(Heap* heap, int v)
{
  while (1)
  {
    int l = 2*v+1, r = 2*v+2, u = l;

    if (l >= heap->size)
    {
      break;
    }

    if (r < heap->size and GetQueueFront(heap->data[r]) < GetQueueFront(heap->data[l]))
    {
      u = r;
    }

    if (GetQueueFront(heap->data[v]) <= GetQueueFront(heap->data[u]))
    {
      break;
    }

    swap(&heap->data[u], &heap->data[v], sizeof(Queue*));
    v = u;
  }
}

Queue* ExtractMin(Heap* heap)
{
  Queue* min = heap->data[0];
  heap->size--;
  heap->data[0] = heap->data[heap->size];
  SiftDown(heap, 0);
  return min;
}

void HeapInsert(Heap* heap, Queue* value)
{
  heap->size++;
  heap->data[heap->size-1] = value;
  SiftUp(heap, heap->size-1);
}

bool IsHeapEmpty(Heap* heap)
{
  return heap->size == 0;
}

int main()
{
  int k;
  scanf("%d", &k);

  Heap* heap = newHeap(k);

  int sizes[k];
  for (int i = 0; i < k; i++)
  {
    scanf("%d", &sizes[i]);
  }

  for (int j = 0; j < k; j++)
  {
    Queue* queue = newQueue(sizes[j]);
    for (int i = 0; i < sizes[j]; i++)
    {
      int value;
      scanf("%d", &value);
      // QueueEnqueue используется для добавления считываемых значений
      // в последовательность, а не для вставки в очередь с приоритетами.
      QueueEnqueue(queue, value);
    }
    // Вот уже здесь мы добавляем последовательность в очередь с приоритетами.
    HeapInsert(heap, queue);
  }

  while (!IsHeapEmpty(heap))
  {
    Queue* queue = ExtractMin(heap);
    printf("%d ", GetQueueFront(queue));
    QueueDequeue(queue);

    if (!IsQueueEmpty(queue))
    {
      HeapInsert(heap, queue);
    }
    else
    {
      deleteQueue(queue);
    }
  }

  deleteHeap(heap);
  return 0;
}

// Если же вы уверены, что исходная программа работает за O(n log n), то можно
// провести "тестовый" прогон с n=10^7 (возможно, n=5*10^7 сможет отработать за
// 30 секунд на сервере) и k<=3. Также, к слову, можно посмотреть примерное
// количество итераций в SiftDown/SiftUp на тестах с большим n и низким k.
