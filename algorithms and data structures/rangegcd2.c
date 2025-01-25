#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <iso646.h>
#include <math.h>

#define MAX_ARRAY_SIZE 300000

/*
 * @idea: https://codeforces.com/blog/entry/18051/
 */

typedef int Type;
typedef Type(*Neutral)();
typedef Type(*Function)(Type, Type);

typedef struct
{
  Function op;
  Neutral zero;

  Type* data;
  int size;
} Segtree;

Segtree* newSegtree(Function op, Neutral zero)
{
  Segtree* tree = (Segtree*)malloc(sizeof(Segtree));
  tree->op = op;
  tree->zero = zero;
  tree->data = (Type*)malloc(2*MAX_ARRAY_SIZE*sizeof(Type));
  for (int i = 0; i < 2*MAX_ARRAY_SIZE; i++)
  {
    tree->data[i] = zero();
  }
  return tree;
}

void deleteSegtree(Segtree* tree)
{
  free(tree->data);
  free(tree);
}

void Build(Segtree* tree, Type* array, int size)
{
  tree->size = size;
  for (int i = 0; i < size; i++)
  {
    tree->data[i+size] = array[i];
  }

  for (int i = size-1; i > 0; i--)
  {
    tree->data[i] = tree->op(tree->data[i << 1], tree->data[i<<1|1]);
  }
}

void Update(Segtree* tree, int position, Type value)
{
  position += tree->size;
  tree->data[position] = value;
  for (; position > 1; position >>= 1)
  {
    tree->data[position>>1] = tree->op(
        tree->data[position],
        tree->data[position^1]
    );
  }
}


Type Query(Segtree* tree, int l, int r)
{
  Type answ = tree->zero();
  for (l += tree->size, r += tree->size; l < r; l >>= 1, r >>= 1)
  {
    if (l&1)
    {
      answ = tree->op(tree->data[l], answ);
      l++;
    }
    if (r&1)
    {
      r--;
      answ = tree->op(tree->data[r], answ);
    }
  }
  return answ;
}

/* --==*==-- */

int gcd(int a, int b)
{
  while (a > 0 and b > 0)
  {
    if (a > b)
    {
      a %= b;
    }
    else
    {
      b %= a;
    }
  }
  return a+b;
}

int zero()
{
  return 0;
}

int main()
{
  Segtree* tree = newSegtree(&gcd, &zero);

  int n, m;
  scanf("%d", &n);

  int* a = (int*)malloc(n*sizeof(int));
  for (int i = 0; i < n; i++)
  {
    scanf("%d", &a[i]);
    a[i] = (int)fabs(a[i]);
  }
  Build(tree, a, n);

  scanf("%d", &m);
  
  for (int i = 0; i < m; i++)
  {
    int l, r;
    scanf("%d %d", &l, &r);
    printf("%d\n", Query(tree, l, r+1));
  }
 
  deleteSegtree(tree);
  free(a);
  return 0;
}
