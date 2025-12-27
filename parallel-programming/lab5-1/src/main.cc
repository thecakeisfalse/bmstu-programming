#include <iostream>
#include <vector>
#include <thread>
#include <random>
#include <chrono>
#include <ranges>
#include <algorithm>
#include <barrier>

using Mat = std::vector<std::vector<int>>;

namespace ranges = std::ranges;
namespace views = std::views;

constexpr int rows = 10000, cols = 10000, num_threads = 8, num_steps = 10;

auto count_alive(const Mat& mat, int x, int y) {
    const static std::vector direction = { -1, 0, 1 };

    return ranges::fold_left(
        views::cartesian_product(direction, direction) | views::filter([](auto d) {
            const auto [dx, dy] = d;
            return dx != dy;
        }) | views::transform([&](auto d) {
            const auto [dx, dy] = d;
            const int rows = mat.size();
            const int cols = mat[0].size();
            return std::make_pair((x + dx + rows) % rows, (y + dy + cols) % cols);
        }) | views::transform([&](auto d) {
            auto [x, y] = d;
            return mat[x][y];
        }),
        0, std::plus<int>()
    );
}

int main() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<int> dis(0, 1);

    const auto matrix = views::iota(0, rows) | views::transform([&](auto _) {
                            return views::iota(0, cols) |
                                   views::transform([&](auto _) { return dis(gen); }) |
                                   ranges::to<std::vector>();
                        }) |
                        ranges::to<Mat>();

    {
        auto result = matrix;
        auto temp = views::iota(0, rows) | views::transform([&](auto _) {
                        return views::iota(0, cols) | views::transform([&](auto _) { return 0; }) |
                               ranges::to<std::vector>();
                    }) |
                    ranges::to<Mat>();

        auto start_time = std::chrono::high_resolution_clock::now();

        for (int step = 0; step < num_steps; step++) {
            std::vector<std::thread> handles;
            std::barrier b(num_threads);

            for (int i = 0; i < num_threads; i++) {
                handles.emplace_back(
                    [&](int i, const Mat& from, Mat& to) {
                        int start_row = i * (rows / num_threads);
                        int end_row = std::min((i + 1) * (rows / num_threads), rows);

                        for (int x = start_row; x < end_row; x++) {
                            for (int y = 0; y < cols; y++) {
                                const auto n = count_alive(from, x, y);
                                to[x][y] = from[x][y] ? (n == 2 || n == 3) : (n == 3);
                            }
                        }

                        b.arrive_and_wait();
                    },
                    i, std::cref(result), std::ref(temp)
                );
            }

            for (auto& handle : handles) {
                handle.join();
            }

            std::swap(result, temp);
        }

        auto end_time = std::chrono::high_resolution_clock::now();
        auto avg = std::chrono::duration<double>(end_time - start_time).count() / num_steps;

        std::cout << "average time: " << avg << std::endl;
    }

    {
        auto result = matrix;
        auto temp = views::iota(0, rows) | views::transform([&](auto _) {
                        return views::iota(0, cols) | views::transform([&](auto _) { return 0; }) |
                               ranges::to<std::vector>();
                    }) |
                    ranges::to<Mat>();

        auto start_time = std::chrono::high_resolution_clock::now();

        for (int step = 0; step < num_steps; step++) {
            for (int x = 0; x < rows; x++) {
                for (int y = 0; y < cols; y++) {
                    const auto n = count_alive(result, x, y);
                    temp[x][y] = result[x][y] ? (n == 2 || n == 3) : (n == 3);
                }
            }

            std::swap(result, temp);
        }

        auto end_time = std::chrono::high_resolution_clock::now();
        auto avg = std::chrono::duration<double>(end_time - start_time).count() / num_steps;

        std::cout << "average time: " << avg << std::endl;
    }

    return 0;
}
