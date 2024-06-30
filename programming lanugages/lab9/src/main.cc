#include <cassert>
#include <iostream>
#include <type_traits>
#include <vector>

using namespace std;

template <typename T, int N, typename = typename std::enable_if_t<std::is_arithmetic_v<T>, T>>
class Matrix {
  private:
    vector<T> m;

  public:
    Matrix() : m(vector<T>(N * N)) {}

    Matrix(vector<T> diag) : m(vector<T>(N * N)) {
        assert(diag.size() == N);

        for (int i = 0; i < N; ++i)
            m[N * (N - 1 - i) + i] = diag[i];
    }

    auto size() { return N; }

    auto &operator()(int i, int j) {
        assert(0 <= i && i < N);
        assert(0 <= j && j < N);
        return m[i * N + j];
    }

    friend auto &operator<<(ostream &output, const Matrix<T, N> &m) {
        for (int i = 0; i < N; ++i) {
            for (int j = 0; j < N; ++j)
                output << m.m[i * N + j] << ' ';
            output << '\n';
        }
        return output;
    }

    friend auto operator*(Matrix<T, N> a, Matrix<T, N> b) {
        Matrix<T, N> result;

        for (int i = 0; i < N; ++i)
            for (int j = 0; j < N; ++j)
                for (int t = 0; t < N; ++t)
                    result(i, j) += a(i, t) * b(t, j);

        return result;
    }

    friend auto operator+(Matrix<T, N> a, Matrix<T, N> b) {
        Matrix<T, N> result;

        for (int i = 0; i < N; ++i)
            for (int j = 0; j < N; ++j)
                result(i, j) += a(i, j) + b(i, j);

        return result;
    }

    auto operator+=(Matrix<T, N> &other) { return (*this = (*this) + other); }
    auto operator*=(Matrix<T, N> &other) { return (*this = (*this) * other); }
};

int main() {
    Matrix<int, 5> m({1, 2, 3, 4, 5});
    Matrix<int, 5> m1({5, 4, 3, 2, 1});

    cout << m << '\n';
    cout << m1 << '\n';

    auto res = (m1 * m) + (m * m1);

    cout << res << '\n';
}
