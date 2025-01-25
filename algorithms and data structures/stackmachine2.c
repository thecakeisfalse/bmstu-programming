#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_MEMORY_SIZE 300000

#define streq(a, b) strcmp(a, b) == 0

typedef char* const string;

int max(int a, int b)
{
  return a > b ? a : b;
}

int min(int a, int b)
{
  return a > b ? b : a;
}

int main()
{
  string command = malloc(10);

  int* mem = malloc(MAX_MEMORY_SIZE*sizeof(int));
  int pointer = 0;

  while (1)
  {
    scanf("%s", command);

    if (streq(command, "CONST"))
    {
      scanf("%d", &mem[pointer]);
      pointer++;
    }
    else if (streq(command, "ADD"))
    {
      pointer--;
      mem[pointer-1] += mem[pointer];
    }
    else if (streq(command, "SUB"))
    {
      pointer--;
      mem[pointer-1] -= mem[pointer];
      mem[pointer-1] *= -1;
    }
    else if (streq(command, "MUL"))
    {
      pointer--;
      mem[pointer-1] *= mem[pointer];
    }
    else if (streq(command, "DIV"))
    {
      pointer--;
      mem[pointer-1] = mem[pointer] / mem[pointer-1];
    }
    else if (streq(command, "MAX"))
    {
      pointer--;
      mem[pointer-1] = max(
        mem[pointer-1],
        mem[pointer]
      );
    }
    else if (streq(command, "MIN"))
    {
      pointer--;
      mem[pointer-1] = min(
        mem[pointer-1],
        mem[pointer]
      );
    }
    else if (streq(command, "NEG"))
    {
      mem[pointer-1] *= -1;
    }
    else if (streq(command, "DUP"))
    {
      mem[pointer] = mem[pointer-1];
      pointer++;
    }
    else if (streq(command, "SWAP"))
    {
      int substitute = mem[pointer-1];
      mem[pointer-1] = mem[pointer-2];
      mem[pointer-2] = substitute;
    }
    else if (streq(command, "END"))
    {
      break;
    }
  }

  printf("%d\n", mem[pointer-1]);

  free(command);
  free(mem);
  return 0;
}
