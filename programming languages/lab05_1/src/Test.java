import java.util.*;
import java.util.stream.Stream;

class Product {
    Integer cost, count;
    String name;
    Product (String name, int count, int cost) {
        this.name = name;
        this.count = count;
        this.cost = cost;
    }

    @Override
    public String toString() {
        return "Product: " + name + " (cost=" + cost.toString() + ", count=" + count.toString() + ")";
    }
}

class ProductTable {
    HashMap<String, Product> Table;
    int total;
    ProductTable() {
        Table = new HashMap<>();
        total = 0;
    }
    void add(Product p) {
        Table.put(p.name, p);
        total += p.count;
    }

    void add(String name, int cost, int count) {
        this.add(new Product(name, count, cost));
    }

    public Stream<String> nameStream(int v) {
        ArrayList<String> result = new ArrayList<>();

        Table.entrySet().stream()
                .filter(x -> x.getValue().count * x.getValue().cost > v)
                .forEach(x -> result.add(x.getKey()));

        return result.stream();
    }

    public Optional<Product> getProduct() {
        Optional<Product> result = Optional.empty();

        Optional<Map.Entry<String, Product>> tmp = Table.entrySet()
                .stream()
                .filter(x -> x.getValue().count * 2 > total)
                .findFirst();

        if (tmp.isPresent()) {
            result = Optional.ofNullable(tmp.get().getValue());
        }
        return result;
    }
}

class NameComparator implements Comparator<String> {
    public int compare(String a, String b) {
        return a.charAt(0) - b.charAt(0);
    }
}

public class Test {
    public static void main(String[] args) {
        ProductTable t = new ProductTable();
        t.add("a", 10, 10);
        t.add("b", 6, 10);
        t.add("c", 2, 5);
        t.add("d", 7, 110);
        t.nameStream(90).sorted(new NameComparator()).forEach(System.out::println);
        System.out.println(t.getProduct().get());
    }
}