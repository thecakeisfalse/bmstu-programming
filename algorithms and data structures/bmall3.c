#include <stdio.h>
#include <stdlib.h>
#include <iso646.h>
#include <string.h>

#define ALPHABET_SIZE 128

typedef char* const string;

int max(int a, int b)
{
  return a > b ? a : b;
}

int main(int argc, char* argv[])
{
  string s = argv[1], t = argv[2];
  const int s_size = strlen(s), t_size = strlen(t);

  int* delta1 = (int*)malloc(ALPHABET_SIZE*sizeof(int));
  for (int i = 0; i < ALPHABET_SIZE; i++)
  {
    delta1[i] = s_size;
  }
  for (int i = 0; i < s_size; i++)
  {
    delta1[s[i]] = s_size-i-1;
  }

  int* suffix = (int*)malloc(s_size*sizeof(int));
  int u = suffix[s_size-1] = s_size-1;
  for (int i = s_size-2; i >= 0; i--)
  {
    while (u < s_size-1 and s[u] != s[i])
    {
      u = suffix[u+1];
    }
    if (s[u] == s[i])
    {
      u--;
    }
    suffix[i] = u;
  }

  int* delta2 = (int*)malloc(s_size*sizeof(int));
  for (int i = 0, v = suffix[0]; i < s_size; i++)
  {
    while (v < i)
    {
      v = suffix[v+1];
    }
    delta2[i] = s_size-i+v;
  }
  for (int i = 0; i < s_size-1; i++)
  {
    int v = i;
    while (v < s_size-1)
    {
      v = suffix[v+1];
      if (s[i] != s[v])
      {
        delta2[v] = s_size-(i+1);
      }
    }
  }

  for (int k = s_size-1; k < t_size; )
  {
    int i = s_size-1;
    for (; t[k] == s[i]; i--, k--)
    {
      if (i == 0)
      {
        printf("%d\n", k);
        break;
      }
    }
    k += max(delta1[t[k]], delta2[i]);
  }

  free(suffix);
  free(delta1);
  free(delta2);

  return 0;
}
