#[derive(Debug)]
struct Equations {
    a: Vec<f64>,
    b: Vec<f64>,
    c: Vec<f64>,
    d: Vec<f64>,
    n: usize,
}

impl Equations {
    pub fn new(a: Vec<f64>, b: Vec<f64>, c: Vec<f64>, d: Vec<f64>) -> Result<Self, String> {
        if c.len() + 1 != b.len() || d.len() != b.len() || a.len() + 1 != b.len() {
            return Err(format!("invalid system of equations"));
        }

        Ok(Self {
            n: b.len(),
            a,
            b,
            c,
            d,
        })
    }

    fn forward(&self) -> Result<(Vec<f64>, Vec<f64>), String> {
        let mut alpha = vec![0_f64; self.n];
        let mut beta = vec![0_f64; self.n];

        if self.b[0] == 0.0 {
            return Err(format!("b_1 is zero"));
        }

        alpha[0] = -self.c[0] / self.b[0];
        beta[0] = self.d[0] / self.b[0];

        for i in 1..self.n {
            let k = self.a[i - 1] * alpha[i - 1] + self.b[i];

            if k == 0.0 {
                return Err(format!("denominator of {i}-th coefficient is zero"));
            }

            if i + 1 != self.n {
                alpha[i] = -self.c[i] / k;
            }

            beta[i] = (self.d[i] - self.a[i - 1] * beta[i - 1]) / k;
        }

        Ok((alpha, beta))
    }

    fn backward(&self, alpha: Vec<f64>, beta: Vec<f64>) -> Vec<f64> {
        let mut x = vec![0_f64; self.n];

        x[self.n - 1] = beta[self.n - 1];

        for i in (0..self.n - 1).rev() {
            x[i] = alpha[i] * x[i + 1] + beta[i];
        }

        x
    }

    pub fn solve(self) -> Result<Vec<f64>, String> {
        let (alpha, beta) = self.forward()?;
        Ok(self.backward(alpha, beta))
    }
}

#[derive(Debug)]
struct DiffEquation {
    p: fn(f64) -> f64,
    q: fn(f64) -> f64,
    f: fn(f64) -> f64,
    a: f64,
    b: f64,
}

impl DiffEquation {
    fn new(p: fn(f64) -> f64, q: fn(f64) -> f64, f: fn(f64) -> f64, a: f64, b: f64) -> Self {
        Self { p, q, f, a, b }
    }
}

trait DiffEquationSolver {
    fn solve(eq: &DiffEquation, n: usize) -> Result<Vec<(f64, f64)>, String>;
}

struct TridiagonalMethod {}

impl DiffEquationSolver for TridiagonalMethod {
    fn solve(eq: &DiffEquation, n: usize) -> Result<Vec<(f64, f64)>, String> {
        let h = 1. / (n as f64);
        let x: Vec<_> = (0..=n).map(|i| (i as f64) * h).collect();

        let p: Vec<_> = x.iter().map(|x| (eq.p)(*x)).collect();
        let q: Vec<_> = x.iter().map(|x| (eq.q)(*x)).collect();
        let f: Vec<_> = x.iter().map(|x| (eq.f)(*x)).collect();

        let a: Vec<_> = (1..n - 1).map(|i| 1. - h / 2. * p[i]).collect();
        let b: Vec<_> = (1..n).map(|i| h * h * q[i] - 2.).collect();
        let c: Vec<_> = (1..n - 1).map(|i| 1. + h / 2. * p[i]).collect();
        let mut d: Vec<_> = (2..n - 1).map(|i| h * h * f[i]).collect();

        d.insert(0, h * h * f[1] - eq.a * (1. - h / 2. * p[0]));
        d.push(h * h * f[n - 1] - eq.b * (1. + h / 2. * p[n - 1]));

        let mut y = Equations::new(a, b, c, d)?.solve()?;

        y.insert(0, eq.a);
        y.push(eq.b);

        Ok(x.into_iter().zip(y.into_iter()).collect())
    }
}

struct ShootingMethod {}

impl DiffEquationSolver for ShootingMethod {
    fn solve(eq: &DiffEquation, n: usize) -> Result<Vec<(f64, f64)>, String> {
        let h = 1. / (n as f64);
        let x: Vec<_> = (0..=n).map(|i| (i as f64) * h).collect();

        let p: Vec<_> = x.iter().map(|x| (eq.p)(*x)).collect();
        let q: Vec<_> = x.iter().map(|x| (eq.q)(*x)).collect();
        let f: Vec<_> = x.iter().map(|x| (eq.f)(*x)).collect();

        let epsilon = h;
        let mut y0 = vec![eq.a, eq.a + epsilon];
        let mut y1 = vec![0., epsilon];

        for i in 1..n {
            let bottom = 1. + p[i] * h / 2.;
            let k2 = 1. - p[i] * h / 2.;

            y0.push((f[i] * h.powi(2) + (2. - q[i] * h.powi(2)) * y0[i] - k2 * y0[i - 1]) / bottom);
            y1.push(((2. - q[i] * h.powi(2)) * y1[i] - k2 * y1[i - 1]) / bottom);
        }

        let c1 = (eq.b - y0[n]) / y1[n];

        Ok(x.into_iter()
            .zip((0..=n).map(|i| y0[i] + c1 * y1[i]))
            .collect())
    }
}

fn main() -> Result<(), String> {
    let analytic = if cfg!(feature = "var") {
        |x| (-5. + 12. * f64::exp(-2. * x) + 8. * f64::exp(3. * x)) / 15.
    } else {
        |x| f64::exp(x)
    };

    let eq = if cfg!(feature = "var") {
        DiffEquation::new(|_| -1., |_| -6., |_| 2., 1., analytic(1.))
    } else {
        DiffEquation::new(|_| 5., |_| -3., |x| 3. * f64::exp(x), 1., analytic(1.))
    };

    let n = 10;
    let f1 = TridiagonalMethod::solve(&eq, n)?;
    let f2 = ShootingMethod::solve(&eq, n)?;

    let mut max_diff1: f64 = 0.;
    let mut max_diff2: f64 = 0.;

    println!("x\t\t\ty\t\t\ty_1\t\t\ty_2\t\t\t|y-y*|_1\t\t|y-y*|_2");

    for (&(x, y1), &(_, y2)) in f1.iter().zip(f2.iter()) {
        let diff1 = (y1 - analytic(x)).abs();
        let diff2 = (y2 - analytic(x)).abs();

        println!(
            "{x:0.15}\t{:0.15}\t{y1:0.15}\t{y2:0.15}\t{diff1:0.15}\t{diff2:0.15}",
            analytic(x)
        );

        max_diff1 = max_diff1.max(diff1);
        max_diff2 = max_diff2.max(diff2);
    }

    println!("||y-y*||_1={max_diff1:.15}");
    println!("||y-y*||_2={max_diff2:.15}");

    Ok(())
}
