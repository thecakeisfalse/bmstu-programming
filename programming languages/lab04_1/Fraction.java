public class Fraction {
    Integer a, b;

    public Fraction(Integer a, Integer b) {
        this.a = a;
        this.b = b;
        this.simplify();
    }

    public Pair<Integer, Integer> toPair() { return new Pair<>(a, b); }

    public void print() { System.out.printf("%d/%d ", this.a, this.b); }

    private Integer GCD(Integer a, Integer b) {
        while (a != 0 && b != 0) {
            if (a > b) a %= b;
            else b %= a;
        }
        return a + b;
    }

    private Integer ABS(Integer a) {
        return (a < 0 ? -a : a);
    }

    private void simplify() {
        int g = GCD(ABS(a), ABS(b));

        if (b < 0) {
            a /= -1;
            b /= -1;
        }

        if (g != 0) {
            b /= g;
            a /= g;
        }
    }

    public Fraction multiply(Fraction other) {
        int A = this.a * other.a;
        int B = this.b * other.b;
        return new Fraction(A, B);
    }
}
