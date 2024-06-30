import java.util.ArrayList;

public class BrokenLine implements Comparable<BrokenLine> {
    private ArrayList<Point> points;
    private int intersections;

    public BrokenLine() {
        this.points = new ArrayList<Point>();
        this.intersections = -1;
    }

    public void addPoint(int x, int y) {
        points.add(new Point(x, y));
    }

    public ArrayList<Point> getPoints() {
        return this.points;
    }

    public void print() {
        System.out.println("Points:");
        for (Point p : this.points)
            System.out.printf("  %d %d\n", p.x, p.y);
        System.out.printf("Total intersections: %d\n", this.getIntersections());
    }

    void calculateIntersections() {
        this.intersections = 0;

        if (this.points.size() == 0) {
            Point p = this.points.get(0);
            if (p.x == 0 || p.y == 0)
                this.intersections = 1;
        }

        for (int i = 0; i < this.points.size()-1; i++) {
            Point p1 = this.points.get(i), p2 = this.points.get(i+1);
            if (p1.x * p2.x <= 0) this.intersections++;
            if (p1.y * p2.y <= 0) this.intersections++;
            if (p1.x * p2.y == p1.y * p2.x) this.intersections--;
            if (p1.x == 0 && p2.x == 0) this.intersections = Integer.MAX_VALUE;
            if (p1.y == 0 && p2.y == 0) this.intersections = Integer.MAX_VALUE;
        }
    }

    int getIntersections() {
        if (this.intersections == -1)
            this.calculateIntersections();
        return this.intersections;
    }

    public int compareTo(BrokenLine other) {
        return this.getIntersections() - other.getIntersections();
    }
}
