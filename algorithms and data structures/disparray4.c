#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iso646.h>

#define streq(a, b) strcmp(a, b) == 0

typedef char* const string;

typedef struct List
{
  int key, value;
  struct List* next;
  struct List* previous;
} List;

List* newList(int key, int value)
{
  List* list = (List*)malloc(sizeof(List));
  list->next = list->previous = NULL;
  list->key = key, list->value = value;
  return list;
}

void deleteList(List* list)
{
  if (list != NULL)
  {
    deleteList(list->next);
    free(list);
  }
}

int listAt(List* list, const int key)
{
  List* current = list;

  while (current != NULL)
  {
    if (current->key == key)
    {
      return current->value;
    }
    current = current->next;
  }

  return 0;
}

int listAssign(List* list, int key, int value)
{
  List* current = list;
  List* previous = NULL;

  while (current != NULL)
  {
    if (current->key == key)
    {
      if (value == 0)
      {
        previous->next = current->next;

        if (current->next != NULL)
        {
          current->next->previous = previous;
        }

        free(current);
      }
      else
      {
        current->value = value;
      }

      break;
    }

    previous = current;
    current = current->next;
  }

  if (current == NULL and value != 0)
  {
    previous->next = newList(key, value);
    previous->next->previous = previous;
  }
}

int main()
{
  int m;
  scanf("%d", &m);

  List** disp = (List**)malloc(m*sizeof(List*));
  for (int i = 0; i < m; i++)
  {
    disp[i] = newList(-1, -1);
  }

  string command = malloc(10);

  while (1)
  {
    scanf("%s", command);

    if (streq(command, "AT"))
    {
      int key;
      scanf("%d", &key);
      printf("%d ", listAt(disp[key % m], key));
    }
    else if (streq(command, "ASSIGN"))
    {
      int key, value;
      scanf("%d %d", &key, &value);
      listAssign(disp[key % m], key, value);
    }
    else if (streq(command, "END"))
    {
      break;
    }
  }

  for (int i = 0; i < m; i++)
  {
    deleteList(disp[i]);
  }
  free(disp);
  free(command);
  return 0;
}
