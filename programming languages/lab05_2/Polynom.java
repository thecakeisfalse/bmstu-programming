import java.util.ArrayList;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Polynom {
    ArrayList<Pair<Double, Integer>> coeffs;
    int size;

    Polynom() {
        coeffs = new ArrayList<>();
        size = 0;
    }

    public void addCoeff(double ak) {
        coeffs.add(new Pair<>(ak, size));
        ++size;
    }

    Optional<Double> calculateValueInPoint(double x) {
        return coeffs.stream()
                     .map(y -> (Math.pow(x, y.getSecond()) * y.getFirst()))
                     .reduce(Double::sum);
    }

    public Stream<Optional<Double>> getPointsOnRange(double a, double b, int n) {
        double s = Math.min(a, b), e = Math.max(a, b);
        double step = (e - s) / (double)n;

        return IntStream.range(0, n + 1)
                        .mapToDouble(x -> (s + step * x))
                        .mapToObj(this::calculateValueInPoint);
    }

    public OptionalDouble getDefiniteIntegral(double a, double b) {
        double eps = 1e-5;
        int n = (int)Math.ceil(Math.abs(b - a) / eps);

        double sign = (a <= b ? 1 : -1);

        return getPointsOnRange(a+eps, b-eps, n)
                .mapToDouble(x -> sign * (x.orElse(0.0) * eps))
                .reduce(Double::sum);
    }
}
