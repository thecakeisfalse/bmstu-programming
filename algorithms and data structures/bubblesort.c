#include <stdio.h>

void bubblesort(unsigned long nel,
                int (*compare)(unsigned long i, unsigned long j),
                void (*swap)(unsigned long i, unsigned long j)) {
    for (int i = 0; nel > 0 && i < nel - i - 1; ++i) {
        for (int j = i; j < nel - i - 1; ++j)
            if (compare(j, j + 1) > 0)
                swap(j, j + 1);

        for (int j = nel - i - 1; j > i; --j)
            if (compare(j, j - 1) < 0)
                swap(j, j - 1);
    }
}
