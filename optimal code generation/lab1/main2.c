#include <stdio.h>
#include <math.h>

typedef struct {
    int x;
    double y;
} Point;

typedef union {
    int x;
    char y;
} Uni;

struct F {
    union {
        int x;
        float y;
    };
    int z;
};

int get_x(struct F f) {
    return f.x;
}

double magnitude(Point p) { return p.x * p.x + p.y * p.y * 2.718; }

Point make_point(int x, double y) {
    Point p = { x, y };
    return p;
}

int apply(int (*fn)(int, int), int a, int b) { return fn(a, b); }

int sign(int a) {
    if (a > 0) {
        return 1;
    } else if (a == 0) {
        return 0;
    } else {
        return -1;
    }
}

int foo(char c) {
    int x = c;
    return x;
}

void bar() {
    return;
}

int add(int a, int b) {
    return a + b;
}

int main(void) {
    Point p = make_point(3, 1.5);
    printf("%.2f\n", magnitude(p));
    printf("%d\n", apply(NULL, 1, 2));

    Uni x = { .x = 12 };
    Uni y = { .y = 'a' };

    printf("%f\n", p.x + 1.2);

    printf("%p\n", &p);

    int arr[] = {1, 2, 3};

    printf("%d\n", arr[1]);

    Point p2 = {1, 2};

    p2.x = 12;


    return 0;
}
