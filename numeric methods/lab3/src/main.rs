const EPS: f64 = 1e-3;

fn square(f: fn(f64) -> f64, a: f64, b: f64, n: usize) -> f64 {
    let h = (b - a) / (n as f64);

    h * (0..n)
        .map(|i| a + h * (i as f64))
        .map(|x| x + h / 2.)
        .map(f)
        .sum::<f64>()
}

fn trapezoid(f: fn(f64) -> f64, a: f64, b: f64, n: usize) -> f64 {
    let h = (b - a) / (n as f64);

    h * ((f(a) + f(b)) / 2. + (1..n).map(|i| a + h * (i as f64)).map(f).sum::<f64>())
}

fn simpson(f: fn(f64) -> f64, a: f64, b: f64, n: usize) -> f64 {
    let h = (b - a) / (n as f64);

    h / 3.
        * (f(a)
            + f(b)
            + 2. * (1..n)
                .map(|i| (i, a + h * (i as f64)))
                .map(|(i, x)| f(x) * if i % 2 == 0 { 1. } else { 2. })
                .sum::<f64>())
}

fn richardson(i_h: f64, i_h2: f64, k: usize) -> f64 {
    (i_h - i_h2) / (2_f64.powf(k as f64) - 1.)
}

struct Result {
    n: usize,
    i_h: f64,
    r: f64,
}

fn calculate(
    method: fn(fn(f64) -> f64, f64, f64, usize) -> f64,
) -> impl Fn(fn(f64) -> f64, f64, f64, usize) -> Result {
    move |f, a, b, k| -> Result {
        let mut n = 1;
        let mut r: f64 = 1.;
        let mut i_h: f64 = 0.;
        let mut result: Option<Result> = None;

        while !(r.abs() < EPS) {
            n *= 2;
            let i_h2 = i_h;
            i_h = method(f, a, b, n);
            r = richardson(i_h, i_h2, k);
            result = Some(Result { n, i_h, r });
        }

        result.unwrap()
    }
}

#[cfg(not(feature = "var"))]
fn analytic(a: f64, b: f64) -> f64 {
    f64::exp(b) - f64::exp(a)
}

#[cfg(feature = "var")]
fn analytic(a: f64, b: f64) -> f64 {
    let f = |x| 3. / 4. * f64::ln(3. - x) + 1. / 4. * f64::ln(x + 1.);
    f(b) - f(a)
}

fn main() {
    #[cfg(not(feature = "var"))]
    let f = f64::exp;

    #[cfg(feature = "var")]
    let f = |x: f64| x / (x.powf(2.) - 2. * x - 3.);

    #[cfg(not(feature = "var"))]
    let (a, b) = (0., 1.);

    #[cfg(feature = "var")]
    let (a, b) = (-0.5, 2.5);

    let r1 = calculate(square)(f, a, b, 2);
    let r2 = calculate(trapezoid)(f, a, b, 2);
    let r3 = calculate(simpson)(f, a, b, 4);
    let i = analytic(a, b);

    println!("Метод\t\t\t\tn\tI*\t\t\tR\t\t\tI*+R\t\t\t|Delta|");
    println!(
        "Метод средних прямоугольников\t{}\t{:.15}\t{:.15}\t{:.15}\t{:.15}",
        r1.n,
        r1.i_h,
        r1.r,
        r1.i_h + r1.r,
        (r1.i_h + r1.r - i).abs()
    );
    println!(
        "Метод трапеций\t\t\t{}\t{:.15}\t{:.15}\t{:.15}\t{:.15}",
        r2.n,
        r2.i_h,
        r2.r,
        r2.i_h + r2.r,
        (r2.i_h + r2.r - i).abs()
    );
    println!(
        "Метод Симпсона\t\t\t{}\t{:.15}\t{:.15}\t{:.15}\t{:.15}",
        r3.n,
        r3.i_h,
        r3.r,
        r3.i_h + r3.r,
        (r3.i_h + r3.r - i).abs()
    );
    println!("Значение интеграла: {i:.15}");
}
