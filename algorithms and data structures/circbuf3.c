#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define streq(a, b) strcmp(a, b) == 0

typedef char* const string;

int min(int a, int b)
{
  return a < b ? a : b;
}

typedef struct
{
  int start, end;
  int size, capacity;
  int* buffer;
} CircBuffer;

CircBuffer* newCircBuffer()
{
  CircBuffer* cb = (CircBuffer*)malloc(sizeof(CircBuffer));
  cb->capacity = 4;
  cb->size = cb->start = cb->end = 0;
  cb->buffer = (int*)malloc(4*sizeof(int));
  return cb;
}

void deleteCircBuffer(CircBuffer* cb)
{
  free(cb->buffer);
  free(cb);
}

bool QueueEmpty(CircBuffer* cb)
{
  return cb->size == 0;
}

int Dequeue(CircBuffer* cb)
{
  int substitute = cb->buffer[cb->start];
  cb->start = (cb->start +1) % cb->capacity;
  cb->size--;
  return substitute;
}

void Resize(CircBuffer* cb)
{

  int* substitute_buffer = (int*)malloc(2*cb->capacity*sizeof(int));

  if (cb->end < cb->start)
  {
    memcpy(substitute_buffer, cb->buffer+cb->end, cb->size*sizeof(int));
  }
  else
  {
    const int shift = cb->capacity - cb->end;
    memcpy(substitute_buffer, cb->buffer+cb->end, shift*sizeof(int));
    memcpy(substitute_buffer+shift, cb->buffer, cb->start*sizeof(int));
  }

  free(cb->buffer);
  cb->buffer = substitute_buffer;

  cb->start = 0;
  cb->end = cb->capacity;
  cb->capacity *= 2;
}

void Enqueue(CircBuffer* cb, int value)
{
  if (cb->capacity == cb->size)
  {
    Resize(cb);
  }
 
  cb->buffer[cb->end] = value;
  cb->size++;
  cb->end = (cb->end+1) % cb->capacity;
}

int main()
{
  string command = malloc(10);
  CircBuffer* cb = newCircBuffer();

  while (1)
  {
    scanf("%s", command);

    if (streq(command, "ENQ"))
    {
      int x;
      scanf("%d", &x);
      Enqueue(cb, x);
    }
    else if (streq(command, "DEQ"))
    {
      printf("%d\n", Dequeue(cb));
    }
    else if (streq(command, "EMPTY"))
    {
      puts(QueueEmpty(cb) ? "true" : "false");
    }
    else if (streq(command, "END"))
    {
      break;
    }
  }

  deleteCircBuffer(cb);
  free(command);
  return 0;
}
