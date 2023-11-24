
#include <stdio.h>

int main() {
    long a = 0, b = 0, x, s;
    for (scanf("%ld", &s); s-- > 0 && (scanf("%d", &x)); a |= (1 << x));
    for (scanf("%ld", &s); s-- > 0 && (scanf("%d", &x)); b |= (1 << x));
    for (int i = 0; i < 32; i++) printf(((a & b) & (1 << i)) ? "%d ": "", i);
}
