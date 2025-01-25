#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#define MAX_STRING_SIZE 1000001

typedef char* const string;

int main()
{
  string input = malloc(MAX_STRING_SIZE);
  fgets(input, MAX_STRING_SIZE, stdin);
  
  const int size = strlen(input);
  int count[26] = {0};

  for (int i = 0; i < size && !isspace(input[i]); i++)
  {
    count[input[i]-'a']++;
  }

  for (int i = 'a'; i <= 'z'; i++)
  {
    for (int j = 0; j < count[i-'a']; j++)
    {
      putchar(i);
    }
  }
  putchar('\n');

  free(input);

  return 0;
}
