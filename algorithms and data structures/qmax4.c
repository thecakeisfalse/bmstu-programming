#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <iso646.h>
#include <limits.h>
#include <string.h>

#define MAX_STACK_SIZE 300000

#define streq(a, b) strcmp(a, b) == 0

typedef char* const string;

int max(int a, int b)
{
  return a > b ? a : b;
}

typedef struct
{
  int pointer;
  int* data;
  int* max;
} MaxStack;

MaxStack* newMaxStack()
{
  MaxStack* ms = (MaxStack*)malloc(sizeof(MaxStack));
  ms->pointer = 0;
  ms->data = (int*)malloc(MAX_STACK_SIZE*sizeof(int));
  ms->max = (int*)malloc(MAX_STACK_SIZE*sizeof(int));
  return ms;
}

void deleteMaxStack(MaxStack* ms)
{
  free(ms->data);
  free(ms->max);
  free(ms);
}

void Push(MaxStack* ms, int value)
{
  ms->data[ms->pointer] = value;
  
  if (ms->pointer == 0)
  {
    ms->max[0] = value;
  }
  else
  {
    ms->max[ms->pointer] = max(value, ms->max[ms->pointer-1]);
  }

  ms->pointer++;
}

int Pop(MaxStack* ms)
{
  ms->pointer--;
  int value = ms->data[ms->pointer];
  return value;
}

bool IsStackEmpty(MaxStack* ms)
{
  return ms->pointer == 0;
}

int GetStackMax(MaxStack* ms)
{
  if (IsStackEmpty(ms))
  {
    return INT_MIN;
  }
  return ms->max[ms->pointer-1];
}

typedef struct
{
  MaxStack* stack1;
  MaxStack* stack2;
} MaxQueue;

MaxQueue* newMaxQueue()
{
  MaxQueue* mq = (MaxQueue*)malloc(sizeof(MaxQueue));
  mq->stack1 = newMaxStack();
  mq->stack2 = newMaxStack();
  return mq;
}

void deleteMaxQueue(MaxQueue* mq)
{
  deleteMaxStack(mq->stack1);
  deleteMaxStack(mq->stack2);
  free(mq);
}

void Enqueue(MaxQueue* mq, int value)
{
  Push(mq->stack1, value);
}

int Dequeue(MaxQueue* mq)
{
  if (IsStackEmpty(mq->stack2))
  {
    while (!IsStackEmpty(mq->stack1))
    {
      Push(mq->stack2, Pop(mq->stack1));
    }
  }

  return Pop(mq->stack2);
}

bool IsQueueEmpty(MaxQueue* mq)
{
  return IsStackEmpty(mq->stack1) and IsStackEmpty(mq->stack2);
}

int Maximum(MaxQueue* mq)
{
  return max(GetStackMax(mq->stack1), GetStackMax(mq->stack2));
}

int main()
{
  string command = malloc(10);
  MaxQueue* mq = newMaxQueue();

  while (1)
  {
    scanf("%s", command);

    if (streq(command, "ENQ"))
    {
      int x;
      scanf("%d", &x);
      Enqueue(mq, x);
    }
    else if (streq(command, "DEQ"))
    {
      printf("%d\n", Dequeue(mq));
    }
    else if (streq(command, "EMPTY"))
    {
      puts(IsQueueEmpty(mq) ? "true" : "false");
    }
    else if (streq(command, "MAX"))
    {
      printf("%d\n", Maximum(mq));
    }
    else if (streq(command, "END"))
    {
      break;
    }
  }

  deleteMaxQueue(mq);
  free(command);
  return 0;
}
