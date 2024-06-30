import java.util.*;

public class Test {
    static ArithmeticProgression genNewArithemticProgression(Random rand) {
        int length = rand.nextInt(1, 10);
        double first = rand.nextInt(-10, 100),
               step = rand.nextInt(1, 20);
        return new ArithmeticProgression(first, step, length);
    }

    public static void main(String[] args) {
        Random rand = new Random();
        ArrayList<ArithmeticProgression> a = new ArrayList<ArithmeticProgression>();
        int m = rand.nextInt(3, 10);
        for (int i = 0; i < m; i++)
            a.add(genNewArithemticProgression(rand));

        Collections.sort(a);

        for (ArithmeticProgression ap : a)
            ap.print();
    }
}

