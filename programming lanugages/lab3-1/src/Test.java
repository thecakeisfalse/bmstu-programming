import java.util.*;

public class Test {
    static BrokenLine genNewBrokenLine(Random rand) {
        BrokenLine bl = new BrokenLine();
        int n = rand.nextInt(1, 10);
        for (int i = 0; i < n; i++) {
            int x = rand.nextInt(-100, 100), y = rand.nextInt(-100, 100);
            bl.addPoint(x, y);
        }
        return bl;
    }

    public static void main(String[] args) {
        Random rand = new Random();
        ArrayList<BrokenLine> a = new ArrayList<BrokenLine>();
        int m = rand.nextInt(1, 10);
        for (int i = 0; i < m; i++)
            a.add(genNewBrokenLine(rand));

        Collections.sort(a);

        for (BrokenLine bl : a)
            bl.print();
    }
}

