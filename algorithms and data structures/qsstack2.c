#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_STACK_SIZE 300000

typedef struct
{
  int low, high;
} Task;

typedef struct
{
  int pointer;
  Task* data;
} TaskStack;

TaskStack* newTaskStack()
{
  TaskStack* ts = (TaskStack*)malloc(sizeof(TaskStack));
  ts->pointer = 0;
  ts->data = (Task*)malloc(MAX_STACK_SIZE*sizeof(Task));
  for (int i = 0; i < MAX_STACK_SIZE; i++)
  {
    Task* task = &ts->data[i];
    task->low = task->high = 0;
  }
  return ts;
}

void deleteTaskStack(TaskStack* ts)
{
  free(ts->data);
  free(ts);
}

void Push(TaskStack* ts, int low, int high)
{
  Task* task = &ts->data[ts->pointer];
  task->low = low;
  task->high = high;
  ts->pointer++;
}

Task Pop(TaskStack* ts)
{
  ts->pointer--;
  Task substitute = ts->data[ts->pointer];
  return substitute;
}

bool IsEmpty(TaskStack* ts)
{
  return ts->pointer == 0;
}

/* --==*==-- */

void Quicksort(int* array, int size)
{
  if (size == 0)
  {
    return;
  }

  TaskStack* ts = newTaskStack();
  Push(ts, 0, size-1);

  while (!IsEmpty(ts))
  {
    Task task = Pop(ts);

    const int pivot = array[task.high];
    int middle = task.low;

    for (int i = task.low; i < task.high; i++)
    {
      if (array[i] < pivot)
      {
        // array[i] <--> array[middle]

        const int substitute = array[i];
        array[i] = array[middle];
        array[middle] = substitute;
        middle++;
      }
    }
 
    const int substitute = array[task.high];
    array[task.high] = array[middle];
    array[middle] = substitute;

    if (task.low < middle-1)
    {
      Push(ts, task.low, middle-1);      
    }

    if (middle+1 < task.high)
    {
      Push(ts, middle+1, task.high);
    }
  }

  deleteTaskStack(ts);
}

int main()
{
  int size = 0;
  scanf("%d", &size);
  int* array = (int*)calloc(size, sizeof(int));
  for (int i = 0; i < size; i++)
  {
    scanf("%d", &array[i]);
  }
  Quicksort(array, size);
  for (int i = 0; i < size; i++)
  {
    printf("%d ", array[i]);
  }
  free(array);
  return 0;
}

