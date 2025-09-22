use lab1::Matrix;

fn rand_matrix_i32(n: usize) -> Matrix<i32> {
    use rand::random_range as rng;

    let mut ans = Matrix::new(n);

    for i in 0..n {
        for j in 0..n {
            ans[i][j] = rng(0..1000);
        }
    }

    ans
}

mod matrix {
    use super::rand_matrix_i32;

    const SIZES: &[usize] = &[50, 100, 500, 1000, 1500];

    #[divan::bench(args = SIZES)]
    fn multiply_by_rows(bencher: divan::Bencher, n: usize) {
        bencher
            .with_inputs(|| {
                let a = rand_matrix_i32(n);
                let b = rand_matrix_i32(n);

                (a, b)
            })
            .bench_values(|(a, b)| a.multiply_by_rows(&b));
    }

    #[divan::bench(args = SIZES)]
    fn multiply_by_cols(bencher: divan::Bencher, n: usize) {
        bencher
            .with_inputs(|| {
                let a = rand_matrix_i32(n);
                let b = rand_matrix_i32(n);

                (a, b)
            })
            .bench_values(|(a, b)| a.multiply_by_cols(&b));
    }

    #[divan::bench(args = SIZES)]
    fn multiply_threads(bencher: divan::Bencher, n: usize) {
        bencher
            .with_inputs(|| {
                let a = rand_matrix_i32(n);
                let b = rand_matrix_i32(n);

                (a, b)
            })
            .bench_values(|(a, b)| a.multiply_threads(&b));
    }

    #[divan::bench(args = SIZES)]
    fn multiply_unsafe(bencher: divan::Bencher, n: usize) {
        bencher
            .with_inputs(|| {
                let a = rand_matrix_i32(n);
                let b = rand_matrix_i32(n);

                (a, b)
            })
            .bench_values(|(a, b)| a.multiply_unsafe(&b));
    }

    #[divan::bench(args = SIZES)]
    fn multiply_unsafe_threads(bencher: divan::Bencher, n: usize) {
        bencher
            .with_inputs(|| {
                let a = rand_matrix_i32(n);
                let b = rand_matrix_i32(n);

                (a, b)
            })
            .bench_values(|(a, b)| a.multiply_unsafe_threads(&b));
    }
}

fn main() {
    divan::main();
}
