#include <stdio.h>
#include <stdlib.h>
#include <iso646.h>
#include <string.h>

typedef char* const string;

/*
 * @doc: https://neerc.ifmo.ru/wiki/index.php?title=Префикс-функция
 */

int main(int argc, char* argv[])
{
  string s = argv[1];
  const int size = strlen(s);
  int* p = (int*)calloc(size, sizeof(int));
  for (int i = 1; i < size; i++)
  {
    int k = p[i-1];
    while (k > 0 and s[i] != s[k])
    {
      k = p[k-1];
    }
    if (s[i] == s[k])
    {
      k++;
    }
    p[i] = k;

    int u = (i+1)-k, v = (i+1) % u;
    if (k > 0 and v == 0)
    {
      printf("%d %d\n", i+1, (i+1) / u);
    }
  }
  free(p);
  return 0;
}
