#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1000

char * read_line() {
    char *s = (char *)calloc(BUFFER_SIZE, sizeof(char));
    char *buffer = (char *)calloc(BUFFER_SIZE, sizeof(char));

    while (fgets(buffer, BUFFER_SIZE, stdin)) {
        int n = strlen(buffer);
        s = (char *)realloc(s, strlen(s)+n+1);

        memset(s+strlen(s), 0, n+1);
        memcpy(s+strlen(s), buffer, n);

        if (buffer[n-1] == '\n')
            break;

        memset(buffer, 0, n);
   }

   s[strlen(s)-1] = '\0';
   free(buffer);

   return s;
}

int main() {
    int alph[26] = {0};
    char * s = read_line();

    for (int i = 0; s[i] != '\0'; ++i)
        ++alph[s[i]-'a'];

    for (char c = 'a'; c <= 'z'; ++c)
        while (alph[c-'a'] --> 0)
            printf("%c", c);

    printf("\n");

    return 0;
}