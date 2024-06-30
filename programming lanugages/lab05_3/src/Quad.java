public class Quad {
    private Point a, b, c, d;

    public Quad(Point a, Point b, Point c, Point d) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.d = d;
    }

    public Double getSurfaceArea() {
        double A = a.distanceTo(b), B = b.distanceTo(c),
               C = c.distanceTo(d), D = d.distanceTo(a);

        double p = (A + B + C + D) / 2;
        return Math.sqrt((p - A) * (p - B) * (p - C) * (p - D));
    }

    public Double getSumOfDiagonals() {
        return a.distanceTo(c) + b.distanceTo(d);
    }

    public String toString() {
        return "Quad: area=" + getSurfaceArea().toString() + ", sum of diagonals=" + getSumOfDiagonals().toString();
    }
}
