#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_STRING_SIZE 200000

typedef char* string;

void swap(void *p1, void *p2, size_t size)
{
  char substitute[size];
  memmove(substitute, p1, size);
  memmove(p1, p2, size);
  memmove(p2, substitute, size);
}

typedef struct Elem
{
    struct Elem* next;
    string word;
} List;

List* newList()
{
  List* list = (List*)malloc(sizeof(List));
  list->word = NULL;
  list->next = NULL;
  return list;
}

List* bsort(List* list)
{
  for (List* head = list; head != NULL; head = head->next)
  {
    for (List* substitute = list; substitute->next; substitute = substitute->next)
    {
      if (strlen(substitute->word) > strlen(substitute->next->word))
      {
        swap(&substitute->word, &substitute->next->word, sizeof(string));
      }
    }
  }
  return list;
}

int main()
{
  string input = (string)malloc(MAX_STRING_SIZE);
  fgets(input, MAX_STRING_SIZE, stdin);

  List* list = newList();
  List* tail = list;

  const int size = strlen(input);
  for (int i = 0; i < size; i++)
  {
    if (isspace(input[i]))
    {
      continue;
    }

    string buffer = (string)malloc(1001);

    int j = 0;
    for (; !isspace(input[i]); i++, j++)
    {
      buffer[j] = input[i];
    }
    buffer[j] = 0;

    if (tail->word == NULL)
    {
      tail->word = buffer;
      if (i+1 != size)
      {
        tail->next = newList();
        tail = tail->next;
      }
    }
  }

  list = bsort(list);

  for (List* tail = list; tail != NULL; )
  {
    printf("%s\n", tail->word);
    free(tail->word);
    List* substitute = tail->next;
    free(tail);
    tail = substitute;
  }

  free(input);

  return 0;
}
