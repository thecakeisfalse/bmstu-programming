use lab1::{FlatMatrix, Matrix};

fn rand_matrix_i32(n: usize) -> Matrix<i32> {
    use rand::random_range as rng;

    let mut ans = Matrix::new(n);

    for i in 0..n {
        for j in 0..n {
            ans[i][j] = rng(1..1000);
        }
    }

    ans
}

fn main() {
    use std::time::Instant;

    let n = 5000;
    let a = rand_matrix_i32(n);
    let b = rand_matrix_i32(n);

    let c_1 = {
        let now = Instant::now();
        let c = a.multiply_by_rows(&b);
        let elapsed = now.elapsed();
        println!("multiply_by_rows: {elapsed:.2?}");
        c
    };

    let c_2 = {
        let now = Instant::now();
        let c = a.multiply_by_cols(&b);
        let elapsed = now.elapsed();
        println!("multiply_by_cols: {elapsed:.2?}");
        c
    };

    let c_3 = {
        let now = Instant::now();
        let c = a.multiply_threads(&b);
        let elapsed = now.elapsed();
        println!("multiply_threads: {elapsed:.2?}");
        c
    };

    let c_4 = {
        let now = Instant::now();
        let c = a.multiply_unsafe(&b);
        let elapsed = now.elapsed();
        println!("multiply_unsafe: {elapsed:.2?}");
        c
    };

    let c_5 = {
        let now = Instant::now();
        let c = a.multiply_unsafe_threads(&b);
        let elapsed = now.elapsed();
        println!("multiply_unsafe_threads: {elapsed:.2?}");
        c
    };

    let c_6 = {
        let a: FlatMatrix<_> = a.clone().into();
        let b: FlatMatrix<_> = b.clone().into();
        let now = Instant::now();
        let c = a.multiply(&b);
        let elapsed = now.elapsed();
        println!("multiply (flat matrix): {elapsed:.2?}");
        c.into()
    };

    let c_7 = {
        let a: FlatMatrix<_> = a.clone().into();
        let b: FlatMatrix<_> = b.clone().into();
        let now = Instant::now();
        let c = a.multiply_threads(&b);
        let elapsed = now.elapsed();
        println!("multiply_threads (flat matrix): {elapsed:.2?}");
        c.into()
    };

    let c = [c_1, c_2, c_3, c_4, c_5, c_6, c_7];

    println!(
        "{}",
        if c.iter().all(|x| *x == c[0]) {
            "CORRECT"
        } else {
            "INCORRECT"
        }
    );
}
