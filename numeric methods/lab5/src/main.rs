fn calculate_ab(f: &[(f64, f64)]) -> (f64, f64) {
    let n = f.len() as f64;

    let sum_x: f64 = f.iter().map(|&(x, _)| x).sum();
    let sum_y: f64 = f.iter().map(|&(_, y)| y).sum();
    let sum_xx: f64 = f.iter().map(|&(x, _)| x * x).sum();
    let sum_xy: f64 = f.iter().map(|&(x, y)| x * y).sum();

    let det = n * sum_xx - sum_x * sum_x;
    let a = (n * sum_xy - sum_x * sum_y) / det;
    let b = (sum_y * sum_xx - sum_x * sum_xy) / det;

    (a, b)
}

fn mse(f: &[(f64, f64)], z: impl Fn(f64) -> f64) -> f64 {
    f.iter().map(|&(x, y)| (y - z(x)).powi(2)).sum::<f64>()
}

fn main() {
    let f: Vec<(f64, f64)> = vec![
        (1., 2.35),
        (1.5, 2.74),
        (2., 5.37),
        (2.5, 6.96),
        (3., 8.52),
        (3.5, 10.52),
        (4., 13.41),
        (4.5, 15.93),
        (5., 17.61),
    ];

    let z = [8.5, 6.2, 3.5];

    let num = [[1, 3, 6], [4, 2, 9], [5, 8, 7]];
    let n = f.len();

    let x_agh = [
        (f[0].0 + f[n - 1].0) / 2.,
        (f[0].0 * f[n - 1].0).sqrt(),
        2. / (1. / f[0].0 + 1. / f[n - 1].0),
    ];

    println!("x_agh = {x_agh:?}");

    let y_agh = [
        (f[0].1 + f[n - 1].1) / 2.,
        (f[0].1 * f[n - 1].1).sqrt(),
        2. / (1. / f[0].1 + 1. / f[n - 1].1),
    ];

    let mut min_delta = 1e9;
    let mut ans = 0;

    for i in 0..3 {
        for j in 0..3 {
            let delta = (z[i] - y_agh[j]).abs();
            if delta < min_delta {
                min_delta = delta;
                ans = num[i][j];
            }
        }
    }

    println!("Лучшая функция: z_{ans} (d={min_delta:?})");

    let lin: Vec<(f64, f64)> = f.iter().map(|&(x, y)| (x.ln(), y.ln())).collect();
    let (b, ln_a) = calculate_ab(&lin);
    let a = ln_a.exp();

    let z = |x: f64| a * x.powf(b);

    println!("\nКоэффициенты:\na = {a:.15}\nb = {b:.15}\n");

    println!("x\ty\tz\t\t|Delta|");
    for &(x, y) in &f {
        let z = z(x);
        let delta = (z - y).abs();
        println!("{x:.2}\t{y:.2}\t{z:.8}\t{delta:.8}");
    }

    let delta = mse(&f, z);
    println!("\nСреднеквадратичное отклонение = {delta:.15}");
}
