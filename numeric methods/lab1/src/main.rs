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

fn f(x: f64) -> f64 {
    f64::exp(x)
}

fn calculate_table(n: usize, a: f64, h: f64) -> (Vec<f64>, Vec<f64>) {
    let mut x = vec![];
    let mut y = vec![];

    for i in 0..=n {
        x.push(a + h * (i as f64));
        y.push(f(*x.last().unwrap()));
    }

    (x, y)
}

fn calculate_free_coeff(y: &[f64], n: usize, h: f64) -> Vec<f64> {
    (1..n)
        .map(|i| 3. * (y[i + 1] - 2. * y[i] + y[i - 1]) / (h * h))
        .collect()
}

fn main() -> Result<(), String> {
    let n = 20;
    let h = 1. / (n as f64);
    let (x, y) = calculate_table(n, 0., h);
    let free = calculate_free_coeff(&y, n, h);

    let mut spline_c =
        Equations::new(vec![1.; n - 2], vec![4.; n - 1], vec![1.; n - 2], free)?.solve()?;

    spline_c.insert(0, 0.);
    spline_c.push(0.);

    let spline_a = y.clone();

    let spline_b = (0..n)
        .map(|i| (y[i + 1] - y[i]) / h - (h / 3.) * (spline_c[i + 1] + 2. * spline_c[i]))
        .collect::<Vec<_>>();

    let spline_d = (0..n)
        .map(|i| (spline_c[i + 1] - spline_c[i]) / (3. * h))
        .collect::<Vec<_>>();

    let g = |v: f64, i: usize| {
        spline_a[i]
            + spline_b[i] * (v - x[i])
            + spline_c[i] * (v - x[i]).powf(2.)
            + spline_d[i] * (v - x[i]).powf(3.)
    };

    for (i, &x) in x.iter().enumerate().take(n) {
        {
            println!(
                "x_i: {x:.15}\tf(x_i): {:.15}\tS(x_i): {:.15}\t|f(x_i) - S(x_i)|: {:.15}",
                f(x),
                g(x, i),
                (f(x) - g(x, i)).abs()
            );
        }
        {
            let x = (i as f64 + 0.5) * h;
            println!(
                "x_i: {x:.15}\tf(x_i): {:.15}\tS(x_i): {:.15}\t|f(x_i) - S(x_i)|: {:.15}",
                f(x),
                g(x, i),
                (f(x) - g(x, i)).abs()
            );
        }
    }

    Ok(())
}
