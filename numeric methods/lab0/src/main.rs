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

fn main() -> Result<(), String> {
    let x = Equations::new(
        vec![1., 1., 1.],
        vec![4., 4., 4., 4.],
        vec![1., 1., 1.],
        vec![5., 6., 6., 5.],
    )?
    .solve()?;

    println!("{x:?}");

    Ok(())
}
