import java.util.Random;
import java.util.stream.IntStream;

public class Test {

    public static Quad newQuad(Random rnd) {
        return new Quad(
                new Point(rnd.nextInt(-10, 10), rnd.nextInt(-10, 10)),
                new Point(rnd.nextInt(-10, 10), rnd.nextInt(-10, 10)),
                new Point(rnd.nextInt(-10, 10), rnd.nextInt(-10, 10)),
                new Point(rnd.nextInt(-10, 10), rnd.nextInt(-10, 10))
        );
    }

    public static void main(String[] args) {
        QuadSet s = new QuadSet();

        Random r = new Random();
        int n = r.nextInt(5, 10);

        IntStream.range(0, n)
                 .mapToObj(x -> newQuad(r))
                 .forEach(s::addQuad);

        IntStream.range(0, 20)
                 .forEach(x -> s.getAllQuadsArea()
                                 .filter(y -> (10 * x <= y && y < 10 * (x+1)))
                                 .forEach(System.out::println));

        System.out.println(s.getMaximumDiagonalsSum().get());
    }
}