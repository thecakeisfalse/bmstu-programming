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

int max(int a, int b)
{
  return a > b ? a : b;
}

typedef struct
{
  int* data;
  int size;
} Heap;

Heap* newHeap(int size)
{
  Heap* heap = (Heap*)malloc(sizeof(Heap));
  heap->size = 0;
  heap->data = (int*)malloc(size*sizeof(int));
  for (int i = 0; i < size; i++)
  {
    heap->data[i] = 0;
  }
  return heap;
}

void deleteHeap(Heap* heap)
{
  free(heap->data);
  free(heap);
}

void SiftUp(Heap* heap, int v)
{
  while (v > 0 and heap->data[v] < heap->data[(v-1)/2])
  {
    swap(&heap->data[v], &heap->data[(v-1)/2], sizeof(int));
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

    if (r < heap->size and heap->data[r] < heap->data[l])
    {
      u = r;
    }

    if (heap->data[v] <= heap->data[u])
    {
      break;
    }

    swap(&heap->data[u], &heap->data[v], sizeof(int));
    v = u;
  }
}

int ExtractMin(Heap* heap)
{
  int min = heap->data[0];
  heap->size--;
  heap->data[0] = heap->data[heap->size];
  SiftDown(heap, 0);
  return min;
}

void Insert(Heap* heap, int value)
{
  heap->size++;
  heap->data[heap->size-1] = value;
  SiftUp(heap, heap->size-1);
}

bool IsHeapEmpty(Heap* heap)
{
  return heap->size == 0;
}

int GetHeapSize(Heap* heap)
{
  return heap->size;
}

int main()
{
  int n, m;
  scanf("%d %d", &n, &m);

  Heap* heap = newHeap(n);

  for (int i = 0; i < m; i++)
  {
    int t0, dt;
    scanf("%d %d", &t0, &dt);

    if (GetHeapSize(heap) >= n)
    {
      t0 = max(t0, ExtractMin(heap));
    }
    
    Insert(heap, t0+dt);
  }

  int answ = 0;
  while (!IsHeapEmpty(heap))
  {
    answ = ExtractMin(heap);
  }

  printf("%d\n", answ);
  deleteHeap(heap);
  return 0;
}
