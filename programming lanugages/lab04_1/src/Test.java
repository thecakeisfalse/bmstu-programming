import java.util.Random;

public class Test {
    public static void main(String[] args) {
        Random r = new Random();
        FractionsSet s = new FractionsSet();

        for (int i = 0; i < 10; ++i) {
            int a = r.nextInt(-10, 10), b = r.nextInt(-10, 10);
            s.addFraction(new Fraction(a, b));
        }
        s.print();
    }
}