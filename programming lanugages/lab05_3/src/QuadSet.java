import java.util.ArrayList;
import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Stream;

public class QuadSet {
    private ArrayList<Quad> quads;

    public QuadSet() {
        quads = new ArrayList<>();
    }

    public void addQuad(Quad q) {
        quads.add(q);
    }

    public Stream<Double> getAllQuadsArea() {
        return quads.stream().map(Quad::getSurfaceArea);
    }

    public Optional<Quad> getMaximumDiagonalsSum() {
        return quads.stream().max(new DiagonalsSumComparator());
    }

    class DiagonalsSumComparator implements Comparator<Quad> {
        public int compare(Quad a, Quad b) {
            return (int)(a.getSumOfDiagonals() - b.getSumOfDiagonals());
        }
    }
}
