#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <iso646.h>

bool IsPower2(int x)
{
  return x and !(x & (x-1));
}

int Count(int* a, int n, int sum)
{
  if (n == 0)
  {
    return IsPower2(sum);
  }
  return Count(a+1, n-1, sum) + Count(a+1, n-1, sum+(*a));
}

int main()
{
  int n;
  scanf("%d", &n);
  int* a = (int*)malloc(n*sizeof(int));
  for (int i = 0; i < n; i++)
  {
    scanf("%d", &a[i]);
  }
  printf("%d\n", Count(a, n, 0));
  free(a);
  return 0;
}
