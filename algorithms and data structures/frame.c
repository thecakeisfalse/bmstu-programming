#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char * argv[]) {
    if (argc != 4) {
        printf("Usage: frame <height> <width> <text>\n");
        return 0;
    }

    int height = atoi(argv[1]), width = atoi(argv[2]), length = strlen(argv[3]);

    if (length > width-2 || height < 3) {
        printf("Error\n");
        return 0;
    }

    int dy = (height + 1) / 2, dx = (width - length + 2) / 2;

    for (int y = 1; y <= height; ++y) {
        for (int x = 1; x <= width; ++x) {
            if ((x == 1 || x == width) || (y == 1 || y == height)) printf("*");
            else if (y == dy && dx <= x && x < dx + length) printf("%c", argv[3][x - dx]);
            else printf(" ");
        }
        printf("\n");
    }

    return 0;
}