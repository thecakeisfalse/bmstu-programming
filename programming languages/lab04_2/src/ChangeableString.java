import java.util.Iterator;

public class ChangeableString implements Iterable<Integer> {
    private int[] count;
    StringBuilder s;

    public ChangeableString(StringBuilder s) {
        this.s = s;
        this.count = new int[26];
    }

    public Iterator<Integer> iterator() {
        for (int c : s.toString().toLowerCase().toCharArray())
            if ('a' <= c && c <= 'z')
                ++this.count[c-'a'];

        return new StringIterator();
    }

    public void print() {
        System.out.println(this.s);

        int ch = 'a';
        for (Integer x : this) {
            if (x > 0) System.out.printf("%c: %d\n", ch, x);
            ch++;
        }
    }

    private class StringIterator implements Iterator<Integer> {
        private int pos;

        public StringIterator() { this.pos = 0; }
        public boolean hasNext() { return this.pos < 26; }
        public Integer next() { return (hasNext() ? count[this.pos++] : -1); }
    }
}
