public class ArithmeticProgression implements Comparable<ArithmeticProgression> {
    private double first, step;
    private int length;

    public ArithmeticProgression(double first, double step, int length) {
        this.first = first;
        this.length = length;
        this.step = step;
    }

    public void print() {
        System.out.printf("First: %f; Step: %f; Length: %d\n", first, step, length);
        System.out.printf("Points in (0, 100): %d\n", this.countPoints());
    }

    public int countPoints() {
        int count = 0;
        double value = this.first;

        for (int k = 0; k < this.length && value < 100; k++, value += step)
            if (0 < value)
                count++;

        return count;
    }

    public int compareTo(ArithmeticProgression other) {
        return this.countPoints() - other.countPoints();
    }
}

