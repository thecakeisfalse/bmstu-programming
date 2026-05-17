int main() {
    int y = 10;
    int z = 1;
    int x = 2 + 3 * y;

    for (; x == z; x = x - 1) {
        z = 10;
    }

    if (z != 10) {
        return 321;
    }

    return 123;
}
