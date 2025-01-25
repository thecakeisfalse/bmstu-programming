#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <iso646.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ARRAY_SIZE 1000000

typedef char* const string;

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
      answ = tree->op(answ, tree->data[l]);
      l++;
    }
    if (r&1)
    {
      r--;
      answ = tree->op(answ, tree->data[r]);
    }
  }
  return answ;
}

/* --==*==-- */

Type zero()
{
  return 0;
}

Type sum(Type a, Type b)
{
  return a ^ b;
}

bool check(Type v)
{
  int count = 0;
  for (int i = 0; i < 26; i++)
  {
    count += (bool)(v & (1 << i));
  }
  return count <= 1;
}

int main()
{
  Segtree* tree = newSegtree(&sum, &zero);

  string s = malloc(MAX_ARRAY_SIZE+1);
  fgets(s, MAX_ARRAY_SIZE+1, stdin);

  const int size = strlen(s);
  s[size-1] = 0;

  Type* a = (Type*)calloc(size, sizeof(Type));
  for (int i = 0; i < size; i++)
  {
    a[i] = 1 << (s[i]-'a');
  }
  Build(tree, a, size);

  string command = malloc(10);
  string buffer = calloc(MAX_ARRAY_SIZE+1, sizeof(char));

  while (1)
  {
    scanf("%s", command);

    if (!strcmp(command, "HD"))
    {
      int l, r;
      scanf("%d %d", &l, &r);
      Type v = Query(tree, l, r+1);

      puts(check(v) ? "YES" : "NO");
    }
    else if (!strcmp(command, "UPD"))
    {
      int i;
      scanf("%d %s", &i, buffer);

      const int buffer_size = strlen(buffer);
      for (int j = 0; j < buffer_size; j++)
      {
        Type new = 1 << (buffer[j]-'a');
        Update(tree, i+j, new);
      }
    }
    else if (!strcmp(command, "END"))
    {
      break;
    }
  }
 
  deleteSegtree(tree);
  free(command);
  free(s);
  free(a);
  free(buffer);

  return 0;
}
