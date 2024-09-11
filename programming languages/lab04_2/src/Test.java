import java.util.*;

public class Test {
    public static String generateString(Random r) {
        String abc = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
        int length = r.nextInt(10, 50);
        char[] text = new char[length];
        for (int i = 0; i < length; i++)
            text[i] = abc.charAt(r.nextInt(abc.length()));
        return new String(text);
    }

    public static void main(String[] args) {
        Random r = new Random();
        StringBuilder b = new StringBuilder(generateString(r));
        ChangeableString s = new ChangeableString(b);
        s.print();
        for (int i = 0; i < 10; ++i)
            b.insert(i, (char)('A' + r.nextInt(0, 26)));
        s.print();
    }
}