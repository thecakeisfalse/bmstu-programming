#include <iostream>
#include <vector>
#include <cmath>
#include <chrono>
#include <mpi.h>
#include <algorithm>
#include <ranges>

using f64 = double;
using usize = size_t;

using Vec = std::vector<f64>;
using Mat = std::vector<Vec>;

namespace views = std::views;
namespace ranges = std::ranges;

auto dot(const Vec& u, const Vec& v) {
    return ranges::fold_left(
        views::zip_transform(std::multiplies<f64>(), u, v), 0, std::plus<f64>()
    );
}

auto norm(const Vec& u) { return std::sqrt(dot(u, u)); }

auto partial_sum(ranges::input_range auto&& range) {
    using value_type = ranges::range_value_t<decltype(range)>;
    value_type sum {};

    return range |  //
           std::views::transform([sum = sum](const auto& x) mutable {
               value_type temp = sum;
               sum += x;
               return temp;
           });
}

struct Solver {
    int rank, n;
    f64 eps;

    Mat local_A;
    Vec b, r, x;

    std::vector<int> sizes, offsets;

    void init(int n, f64 eps) {
        this->eps = eps;
        this->n = n;

        int size;

        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Comm_size(MPI_COMM_WORLD, &size);

        this->sizes =
            views::iota(0, size) |
            views::transform([&](auto i) { return (n / size) + (usize) (i < n % size); }) |
            ranges::to<std::vector<int>>();

        this->offsets = partial_sum(this->sizes) | ranges::to<std::vector<int>>();

        local_A.assign(sizes[rank], Vec(n, 1.0));

        for (usize i = 0; i < sizes[rank]; i++) {
            usize j = offsets[rank] + i;
            local_A[i][j] = 2.0;
        }

        b.assign(n, n + 1);
        x.assign(n, 0.0);
        r.assign(n, 0.0);
    }

    void solve() {
        auto start = MPI_Wtime();

        const auto b_norm = norm(b);

        while (true) {
            Vec temp(n);

            auto Ax_local = views::iota(0, sizes[rank]) |
                            views::transform([&](auto i) { return dot(local_A[i], x); }) |
                            ranges::to<Vec>();

            MPI_Allgatherv(
                Ax_local.data(), sizes[rank], MPI_DOUBLE, temp.data(), sizes.data(), offsets.data(),
                MPI_DOUBLE, MPI_COMM_WORLD
            );

            for (int i = 0; i < n; i++) {
                r[i] = temp[i] - b[i];
            }

            auto Ar_local = views::iota(0, sizes[rank]) |
                            views::transform([&](auto i) { return dot(local_A[i], r); }) |
                            ranges::to<Vec>();

            MPI_Allgatherv(
                Ar_local.data(), sizes[rank], MPI_DOUBLE, temp.data(), sizes.data(), offsets.data(),
                MPI_DOUBLE, MPI_COMM_WORLD
            );

            auto r_dot_Ar = dot(r, temp);
            auto Ar_dot_Ar = dot(temp, temp);

            f64 tau = r_dot_Ar / Ar_dot_Ar;

            Vec x_local(sizes[rank]);
            for (usize i = 0; i < sizes[rank]; i++) {
                usize global_i = offsets[rank] + i;
                x_local[i] = x[global_i] - tau * r[global_i];
            }

            MPI_Allgatherv(
                x_local.data(), sizes[rank], MPI_DOUBLE, x.data(), sizes.data(), offsets.data(),
                MPI_DOUBLE, MPI_COMM_WORLD
            );

            auto relative_error = norm(r) / b_norm;

            if (relative_error < eps) {
                break;
            }
        }

        if (rank == 0) {
            auto end = MPI_Wtime();

            std::cout << (end - start) << std::endl;

            bool correct = ranges::all_of(x, [&](auto v) { return std::abs(v - 1.0) < eps; });

            if (correct) {
                std::cout << "Correct" << std::endl;
            } else {
                std::cout << "Incorrect" << std::endl;
            }
        }
    }
};

int main(int argc, char* argv[]) {
    MPI_Init(&argc, &argv);

    Solver solver;
    solver.init(32678, 1e-6);
    solver.solve();

    MPI_Finalize();

    return 0;
}
