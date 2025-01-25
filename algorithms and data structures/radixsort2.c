#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef union Int32 {
  int x;
  unsigned char bytes[4];
} Int32;

int main()
{
  int n = 0;
  scanf("%d", &n);

  Int32* a = (Int32*)calloc(n, sizeof(Int32));
  for (int i = 0; i < n; i++)
  {
    scanf("%d", &a[i].x);
    a[i].bytes[3] ^= 128;
  }

  Int32* substitute = (Int32*)calloc(n, sizeof(Int32));
  
  for (int j = 0; j < 4; j++)
  {
    int count[256] = {0};

    for (int i = 0; i < n; i++)
    {
      count[a[i].bytes[j]]++;
    }
    for (int i = 1; i < 256; i++)
    {
      count[i] += count[i-1];
    }
    for (int i = n-1; i >= 0; i--)
    {
      substitute[count[a[i].bytes[j]]-1] = a[i];
      count[a[i].bytes[j]]--;
    }
    for (int i = 0; i < n; i++)
    {
      a[i] = substitute[i];
    }
  }

  for (int i = 0; i < n; i++)
  {
    a[i].bytes[3] ^= 128;
    printf("%d ", a[i].x);
  }
  printf("\n");

  free(a);
  free(substitute);

  return 0;
}

