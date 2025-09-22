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

    println!(
        "{}",
        if c_1 == c_2 && c_2 == c_3 && c_3 == c_4 && c_4 == c_5 {
            "CORRECT"
        } else {
            "INCORRECT"
        }
    );
}
