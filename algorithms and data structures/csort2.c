#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iso646.h>

#define MAX_WORD_COUNT 1000

typedef char* string;
typedef string const cstring;

void csort(cstring src, string dest)
{
  const int size = strlen(src);

  string* a = (string*)malloc(MAX_WORD_COUNT*sizeof(string));
  int count = 0;

  for (int i = 0; i < size; i++)
  {
    if (isspace(src[i]))
    {
      continue;
    }

    string buffer = a[count] = (string)calloc(1001, sizeof(char));
    
    for (int j = 0; i < size and !isspace(src[i]); i++, j++)
    {
      buffer[j] = src[i];
    }

    count++;
  }

  int *compares = (int*)calloc(count, sizeof(int));
  
  for (int i = 0; i < count; i++)
  {
    for (int j = i+1; j < count; j++)
    {
      if (strlen(a[j]) < strlen(a[i]))
      {
        compares[i]++;
      }
      else
      {
        compares[j]++;
      }
    }
  }

  int *order = (int*)malloc(count*sizeof(int));
  for (int i = 0; i < count; i++)
  {
    order[compares[i]] = i;
  }

  int shift = 0;
  for (int i = 0; i < count; i++, shift++)
  {
    string word = a[order[i]];
    const int size = strlen(word);

    for (int j = 0; j < size; j++, shift++)
    {
      dest[shift] = word[j];
    }

    free(word);

    if (i+1 != count)
    {
      dest[shift] = ' ';
    }
    else
    {
      dest[shift] = '\0';
    }
  }

  free(order);
  free(compares);
  free(a);
}

int main()
{
  string src = malloc(1001), dest = calloc(1001, sizeof(char));
  fgets(src, 1001, stdin);
  csort(src, dest);
  puts(dest);
  free(src);
  free(dest);
  return 0;
}
