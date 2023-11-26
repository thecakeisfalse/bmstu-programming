#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 1000

int wcount(char * s) {
    char c = ' ';
    int size = strlen(s), count = 0;

    for (int i = 0; i < size; ++i) {
        if (s[i] != ' ' && c == ' ')
            ++count;

        c = s[i];
    }

    return count;
}

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
    char * s = read_line();
    printf("%d\n", wcount(s));
    free(s);
    return 0;
}