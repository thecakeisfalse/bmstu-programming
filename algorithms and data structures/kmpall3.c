#include <stdio.h>
#include <stdlib.h>
#include <iso646.h>
#include <string.h>

typedef char* const string;

/*
 * @doc: https://neerc.ifmo.ru/wiki/index.php?title=Префикс-функция
 * @doc: https://neerc.ifmo.ru/wiki/index.php?title=Алгоритм_Кнута-Морриса-Пратта
 */

int main(int argc, char* argv[])
{
  string s = argv[1], t = argv[2];
  const int s_size = strlen(s), t_size = strlen(t);

  string buffer = (string)malloc(s_size+t_size+1);
  const int buffer_size = s_size+t_size+1;

  memcpy(buffer, s, s_size);
  buffer[s_size] = '#';
  memcpy(buffer+s_size+1, t, t_size);

  int* p = (int*)calloc(buffer_size, sizeof(int));
  for (int i = 1; i < buffer_size; i++)
  {
    int k = p[i-1];
    while (k > 0 and buffer[i] != buffer[k])
    {
      k = p[k-1];
    }
    if (buffer[i] == buffer[k])
    {
      k++;
    }
    p[i] = k;
  }

  for (int i = 0; i < t_size; i++)
  {
    if (p[s_size+i+1] == s_size)
    {
      printf("%d ", i+1-s_size);
    }
  }
  printf("\n");

  free(p);
  free(buffer);

  return 0;
}
