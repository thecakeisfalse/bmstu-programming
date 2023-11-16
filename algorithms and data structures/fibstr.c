#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *fibstr(int n) {
    size_t a[n + 1];
    a[0] = 0;
    a[1] = 1;

    for (int i = 2; i <= n; ++i)
        a[i] = a[i - 1] + a[i - 2];

    char *ans = (char *)calloc(a[n] + 1, sizeof(char));

    switch (n) {
    case 1:
        strcpy(ans, "a");
        break;
    case 2:
        strcpy(ans, "b");
        break;
    default:
        strcpy(ans + a[n] - a[3], "ab");
        for (int i = 4; i <= n; ++i)
            memcpy(ans + a[n] - a[i], ans + a[n] - a[i - 2], a[i - 2]);
    }

    return ans;
}

int main() {
    int n;
    scanf("%d", &n);
    char *s = fibstr(n);
    printf("%s\n", s);
    free(s);
    return 0;
}

/*

-> ....a (prev)
   ....b (cur)
...ab (ans)

-> ....b (prev)
   ...ab (cur)
..bab (ans)

-> ...ab (prev)
   ..bab (cur)
abbab (ans)

*/
