#include <iostream>
#include <vector>
#include <cmath>
#include <chrono>
#include <algorithm>

using f64 = double;
using usize = size_t;

using Vec = std::vector<f64>;
using Mat = std::vector<Vec>;

auto dot(const Vec& u, const Vec& v) {
    f64 result {};
#pragma omp parallel for reduction(+ : result)
    for (usize i = 0; i < u.size(); i++) {
        result += u[i] * v[i];
    }
    return result;
}

auto norm(const Vec& u) { return sqrt(dot(u, u)); }

struct Solver {
    int rank, n;
    double eps;

    Mat A;
    Vec b, r, x;

    std::vector<int> sizes, offsets;

    void init(int n, double eps) {
        this->eps = eps;
        this->n = n;

        A.assign(n, Vec(n, 1.0));

#pragma omp parallel for
        for (usize i = 0; i < n; i++) {
            A[i][i] = 2.0;
        }

        b.assign(n, n + 1);
        x.assign(n, 0.0);
        r.assign(n, 0.0);
    }

    void matvec(const Vec& x, Vec& y) {
#pragma omp parallel for
        for (int i = 0; i < n; i++) {
            f64 sum {};
            for (int j = 0; j < n; j++) {
                sum += A[i][j] * x[j];
            }
            y[i] = sum;
        }
    }

    void solve() {
        auto start = std::chrono::high_resolution_clock::now();

        const auto b_norm = norm(b);
        Vec Ax(n), Ar(n);

        while (true) {
            matvec(x, Ax);

#pragma omp parallel for
            for (int i = 0; i < n; i++) {
                r[i] = Ax[i] - b[i];
            }

            matvec(r, Ar);

            f64 r_dot_Ar = 0.0, Ar_dot_Ar = 0.0;

#pragma omp parallel for reduction(+ : r_dot_Ar, Ar_dot_Ar)
            for (int i = 0; i < n; i++) {
                r_dot_Ar += r[i] * Ar[i];
                Ar_dot_Ar += Ar[i] * Ar[i];
            }

            f64 tau = r_dot_Ar / Ar_dot_Ar;

#pragma omp parallel for
            for (int i = 0; i < n; i++) {
                x[i] = x[i] - tau * r[i];
            }

            f64 relative_error = norm(r) / b_norm;

            if (relative_error < eps) {
                break;
            }
        }

        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> elapsed = end - start;

        std::cout << elapsed << std::endl;

        bool correct = true;

#pragma omp parallel for reduction(&& : correct)
        for (int i = 0; i < n; i++) {
            correct = correct && (std::abs(x[i] - 1.0) <= eps);
        }

        if (correct) {
            std::cout << "Correct" << std::endl;
        } else {
            std::cout << "Incorrect" << std::endl;
        }
    }
};

int main(int argc, char* argv[]) {
    Solver solver;
    solver.init(32678, 1e-6);
    solver.solve();

    return 0;
}
