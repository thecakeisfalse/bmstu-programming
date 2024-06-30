import java.util.Random;

public class Test {
    public static void main(String[] args) {
        Random rand = new Random();
        int n = 5 ; //+ rand.nextInt(10);
        BinaryRelation bin = new BinaryRelation(n);
        System.out.printf("Current size: %d\n", n+1);

        for (int i = 0; i < n; i++) {
            int x = rand.nextInt(n+1), y = rand.nextInt(n+1);
            System.out.printf("Adding new pair: (%d, %d)\n", x, y);
            bin.addPair(x, y);
        }

        System.out.println("Relations matrix:");
        bin.printRelations();

        bin.computeTransitiveClosure();
        System.out.println("Transitive closure:");
        System.out.println(bin.toString());

        for (int i = 0; i < n / 2; i++) {
            int x = rand.nextInt(n+1), y = rand.nextInt(n+1);
            System.out.printf("Pair (%d, %d) in relation? %s\n", x, y, bin.isPairInRelations(x, y) ? "yes" : "no");
        }
    }
}
