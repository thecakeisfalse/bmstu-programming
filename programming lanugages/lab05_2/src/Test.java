public class Test {
    public static void main(String[] args) {
        Polynom p = new Polynom();
        p.addCoeff(1);
        p.addCoeff(0);
        p.addCoeff(-1);

        double a = -7, b = 7;
        int n = 5;

        p.getPointsOnRange(a, b, n).forEach(x -> System.out.println(x.get()));
        System.out.println(p.getDefiniteIntegral(a, b).getAsDouble());
    }
}