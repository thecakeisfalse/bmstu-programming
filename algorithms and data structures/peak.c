
int is_peak(unsigned long i, unsigned long j,
            int (*less)(unsigned long i, unsigned long j)) {
    return less(j, i) || (less(i, j) == less(j, i));
}

unsigned long peak(unsigned long nel,
                   int (*less)(unsigned long i, unsigned long j)) {
    if (nel == 1)
        return 0;

    if (is_peak(nel - 1, nel - 2, less))
        return nel - 1;

    if (is_peak(0, 1, less))
        return 0;

    for (unsigned long long i = 1; i < nel - 1; ++i)
        if (is_peak(i, i - 1, less) && is_peak(i, i + 1, less))
            return i;
}
