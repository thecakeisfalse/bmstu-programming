use crate::vector::Vector;

mod vector;

const EPS: f64 = 1e-3;

pub struct ScalarFunction {
    n: usize,
    inner: fn(&Vector) -> f64,
}

impl ScalarFunction {
    pub fn new(n: usize, inner: fn(&Vector) -> f64) -> Self {
        Self { n, inner }
    }

    pub fn v(&self, x: &Vector) -> f64 {
        (self.inner)(x)
    }

    pub fn gradient(&self, x: &Vector) -> Vector {
        assert_eq!(x.len(), self.n);

        let h = 1e-6;
        let fx = self.v(x);

        let mut grad = vec![0.; self.n];
        let mut x_h = x.clone();

        for i in 0..self.n {
            x_h[i] += h;
            grad[i] = (self.v(&x_h) - fx) / h;
            x_h[i] -= h;
        }

        Vector::new(grad)
    }
}

fn phi(f: &ScalarFunction, x: &Vector, g: &Vector, t: f64) -> f64 {
    let xn = Vector::new((0..f.n).map(|i| x[i] - t * g[i]).collect());
    f.v(&xn)
}

fn ternary_search(f: &ScalarFunction, x: &Vector, g: &Vector) -> f64 {
    let (mut l, mut r) = (0.0_f64, 1e6_f64);

    while r - l > 1e-10 {
        let m1 = l + (r - l) / 3.;
        let m2 = r - (r - l) / 3.;
        if phi(f, x, g, m1) < phi(f, x, g, m2) {
            r = m2;
        } else {
            l = m1;
        }
    }

    (l + r) / 2.
}

fn deriv_search(f: &ScalarFunction, x: &Vector, g: &Vector) -> f64 {
    let phi1 = -g.norm2();

    let h = 1e-6;

    let phi_0 = phi(f, x, g, 0.);
    let phi_ph = phi(f, x, g, h);
    let phi_mh = phi(f, x, g, -h);

    let phi2 = (phi_ph - 2. * phi_0 + phi_mh) / h.powi(2);

    if phi2.abs() < 1e-15 || phi2 <= 0.0 {
        ternary_search(f, x, g)
    } else {
        -phi1 / phi2
    }
}

struct DescendResult<T> {
    n: usize,
    x_min: Vector,
    y_min: T,
}

fn descent(f: &ScalarFunction, x0: Vector) -> DescendResult<f64> {
    let mut x = x0;
    let mut n = 0;

    for _ in 0..=10000 {
        let g = f.gradient(&x);
        n += 1;

        if g.norm() < EPS {
            break;
        }

        let t = if cfg!(feature = "ternary") {
            ternary_search(f, &x, &g)
        } else {
            deriv_search(f, &x, &g)
        };

        for i in 0..f.n {
            x[i] -= t * g[i];
        }
    }

    let fx = f.v(&x);
    DescendResult {
        n,
        x_min: x,
        y_min: fx,
    }
}

fn main() {
    let f = ScalarFunction::new(2, |v| {
        v[0] + 2. * v[1] + 4. * (1. + v[0].powi(2) + v[1].powi(2)).sqrt()
    });

    let x0 = Vector::new(vec![0., 0.]);
    let r = descent(&f, x0);

    println!("Всего итераций: {:?}", r.n);

    println!("min(f) = f(x*) = f{} = {:.8}", r.x_min, r.y_min);

    let sq11 = (11 as f64).sqrt();
    println!(
        "Аналитическое решение:\nmin(f) = f(~x) = f({:.15}, {:.15}) = {:.15}",
        -1. / sq11,
        -2. / sq11,
        sq11
    );

    println!("|f(x*) - f(~x)| = {:.15}", (sq11 - r.y_min).abs());
}
