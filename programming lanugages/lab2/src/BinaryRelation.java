public class BinaryRelation {
    private int size;
    private int[][] relations;

    public BinaryRelation(int n) {
        this.size = n;
        this.relations = new int[n+1][n+1];
    }

    public void addPair(int x, int y) {
        if (Math.min(x, y) < 0 || Math.max(x, y) > this.size) {
            System.out.printf("Can't add pair, invalid values (%d, %d).\n", x, y);
        } else {
            relations[x][y] = 1;
        }
    }

    public boolean isPairInRelations(int x, int y) {
        if (Math.min(x, y) < 0 || Math.max(x, y) > this.size) {
            System.out.printf("Invalid pair (%d, %d).\n", x, y);
            return false;
        } else {
            return relations[x][y] == 1;
        }
    }

    // https://neerc.ifmo.ru/wiki/index.php?title=Алгоритм_Флойда
    public void computeTransitiveClosure() {
        for (int k = 0; k <= this.size; k++) {
            for (int i = 0; i <= this.size; i++) {
                for (int j = 0; j <= this.size; j++) {
                    this.relations[i][j] |= (this.relations[i][k] & this.relations[k][j]);
                }
            }
        }
    }

    public String toString() {
        String result = "";
        for (int i = 0; i <= this.size; i++) {
            for (int j = 0; j <= this.size; j++) {
                result += (relations[i][j] == 0) ? "0" : "1";
                result += (j != this.size) ? " " : "";
            }
            result += (i != this.size) ? "\n" : "";
        }
        return result;
    }

    public void printRelations() {
        System.out.println(this.toString());
    }
}
