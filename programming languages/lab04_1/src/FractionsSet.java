import java.util.ArrayList;
import java.util.Iterator;

public class FractionsSet implements Iterable<Pair<Fraction, Fraction>> {
    private ArrayList<Fraction> fractions;

    public FractionsSet() {
        this.fractions = new ArrayList<>();
    }

    public void addFraction(Fraction f) {
        this.fractions.add(f);
    }

    public void print() {
        System.out.println("Current fractions:");
        for (Fraction f : fractions) {
            f.print();
            System.out.println();
        }

        System.out.println("Pairs with * of 1:");
        int count = 0;
        for (Pair<Fraction, Fraction> f : this) {
            System.out.printf("%d: ", ++count);
            f.getFirst().print();
            f.getSecond().print();
            System.out.println();
        }
    }

    public Iterator<Pair<Fraction, Fraction>> iterator() { return new SetIterator(); }

    private class SetIterator implements Iterator<Pair<Fraction, Fraction>> {
        private int pos1, pos2;
        private boolean hasPairs, recalculate;

        public SetIterator() {
            this.hasPairs = false;
            this.recalculate = true;
            this.pos1 = this.pos2 = 0;
        }

        public void calculateNextPair() {
            if (!recalculate)
                return;

            recalculate = false;

            if (pos2 >= pos1) {
                pos2 = 0;
                pos1++;
            }

            hasPairs = false;
            for (; pos1 < fractions.size(); ++pos1) {
                Fraction f = fractions.get(pos1);
                for (; pos2 < pos1; ++pos2) {
                    Fraction g = fractions.get(pos2);
                    Pair<Integer, Integer> r = g.multiply(f).toPair();
                    if (r.getFirst() == 1 && r.getSecond() == 1) {
                        hasPairs = true;
                        break;
                    }
                }

                if (hasPairs)
                    break;

                pos2 = 0;
            }
        }

        public boolean hasNext() {
            calculateNextPair();
            return hasPairs;
        }

        public Pair<Fraction, Fraction> next() {
            calculateNextPair();

            if (hasPairs) {
                recalculate = true;
                return new Pair<>(fractions.get(pos1), fractions.get(pos2++));
            }

            return new Pair<>(new Fraction(0, 0), new Fraction(0, 0));
        }
    }
}
