#include <stdio.h>

typedef unsigned long long ullong;

ullong mulmod(ullong a, ullong b, ullong mod) {
    if (b == 0)
        return 0;
    
    return ((2 * mulmod(a % mod, b / 2, mod)) % mod + ((a % mod) * (b % 2)) % mod) % mod;
}

int main() {
    ullong a, b, mod;
    scanf("%llu %llu %llu", &a, &b, &mod);
    printf("%llu\n", mulmod(a, b, mod));
    return 0;
}
