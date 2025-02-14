public class Point {
    private Integer x, y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    double distanceTo(Point other) {
        return Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2));
    }

    Point vectorTo(Point other) {
        return new Point(x - other.x, y - other.y);
    }

    Integer getX() { return x; }
    Integer getY() { return y; }
}