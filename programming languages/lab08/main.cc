#include <cassert>
#include <iostream>
#include <stack>
#include <vector>

using namespace std;

constexpr int INF = 1e9;

template <typename T, int N> class Queue {
  private:
    int max_size, current_size;
    vector<T> s1, s2;

  public:
    Queue() : max_size(N), current_size(0) {}

    void enqueue(T value) {
        assert(current_size != max_size);
        s1.push_back(value);
        current_size++;
    }

    T dequeue(void) {
        if (s2.size() == 0) {
            while (!s1.empty()) {
                s2.push_back(s1.back());
                s1.pop_back();
            }
        }

        current_size--;

        auto res = s2.back();
        s2.pop_back();
        return res;
    }

    bool empty() { return s1.size() + s2.size() == 0; }
};

template <int N> class Queue<int, N> {
  private:
    int max_size;
    int current_size;

    vector<pair<int, int>> s1, s2;

    void push(vector<pair<int, int>> &s, int value) {
        int prev = s.size() ? s.back().second : -INF;
        s.emplace_back(value, max(prev, value));
    }

    int pop(vector<pair<int, int>> &s) {
        if (s.empty())
            return -INF;
        int res = s.back().first;
        s.pop_back();
        return res;
    }

  public:
    Queue() : max_size(N), current_size(0) {}

    int maximum() {
        return max(s1.size() ? s1.back().second : -INF, s2.size() ? s2.back().second : -INF);
    }

    void enqueue(int value) {
        assert(current_size != max_size);
        current_size++;
        push(s1, value);
    }

    int dequeue(void) {
        if (s2.empty())
            while (!s1.empty())
                push(s2, pop(s1));

        if (s2.size() > 0)
            return pop(s2);

        current_size--;

        return -INF;
    }

    bool empty() { return s1.size() + s2.size() == 0; }
};

int main() {
    Queue<int, 5> q;
    q.enqueue(12);
    q.enqueue(15);
    q.enqueue(5);
    q.enqueue(-5);
    q.enqueue(14);
    // q.enqueue(5);
    cout << q.maximum() << '\n';
    cout << q.dequeue() << '\n';
    cout << q.maximum() << '\n';
    cout << q.dequeue() << '\n';
    cout << q.maximum() << '\n';

    Queue<string, 4> q1;
    q1.enqueue("Hello, world");
    q1.enqueue("Test string");
    q1.enqueue("lkjdkfksd");
    cout << q1.dequeue() << '\n';
    cout << q1.dequeue() << '\n';
    cout << q1.dequeue() << '\n';

    return 0;
}