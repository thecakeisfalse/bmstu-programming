#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_STACK_SIZE 10000000

#define PUSH(x) { stack[sp] = x; ++sp; }
#define POP() stack[--sp]
#define CONST { int x; scanf("%d", &x); PUSH(x) }
#define ADD { PUSH(POP() + POP()); }
#define SUB { PUSH(POP() - POP()); }
#define MUL { PUSH(POP() * POP()); }
#define DIV { PUSH(POP() / POP()); }
#define MAX { PUSH(fmax(POP(), POP())); }
#define MIN { PUSH(fmin(POP(), POP())); }
#define NEG { PUSH(-POP()); }
#define DUP { int x = POP(); PUSH(x); PUSH(x); }
#define SWAP { int x = POP(), y = POP(); PUSH(x); PUSH(y); }
#define END { printf("%d\n", POP()); break; }
#define COMMAND(cmd) { if (strcmp(s, #cmd) == 0) cmd; }

int main() {
    int * stack = (int *)calloc(MAX_STACK_SIZE, sizeof(int)), sp = 0, x;
    char s[6] = {0};

    while (1) {
        scanf("%s", s);
        COMMAND(CONST);
        COMMAND(ADD);
        COMMAND(SUB);
        COMMAND(MUL);
        COMMAND(DIV);
        COMMAND(MAX);
        COMMAND(MIN);
        COMMAND(NEG);
        COMMAND(DUP);
        COMMAND(SWAP);
        COMMAND(END);
    }

    free(stack);

    return 0;
}