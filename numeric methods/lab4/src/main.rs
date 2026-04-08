mod matrix;
mod vector;

use matrix::Matrix;
use vector::Vector;

const EPS: f64 = 1e-5;

#[derive(Debug)]
struct Function {
    n: usize,
    inner: fn(&Vector) -> Vector,
}

impl Function {
    fn new(n: usize, inner: fn(&Vector) -> Vector) -> Self {
        Self { n, inner }
    }

    fn jacobian(&self, x: &Vector) -> Matrix {
        assert_eq!(self.n, x.len());

        let mut jacobian = Matrix::zero(self.n);
        let mut x_h = x.clone();
        let h = 1e-6;

        for i in 0..self.n {
            x_h[i] += h;

            let d = (self.v(&x_h) - self.v(x)) / h;

            for j in 0..self.n {
                jacobian[(j, i)] = d[j];
            }

            x_h[i] -= h;
        }

        jacobian
    }

    #[inline]
    fn v(&self, x: &Vector) -> Vector {
        (self.inner)(x)
    }
}

fn newton(f: Function, x0: Vector, iters: usize) -> Option<Vector> {
    let mut x = x0;

    println!("n\tx\t\t\t\tdx\t\t\t\t|dx|");

    for n in 1..=iters {
        let j = f.jacobian(&x);
        let fx = -f.v(&x);
        let dx: Vector = j.gauss(&fx)?;
        x += &dx;

        println!("{n}\t{x}\t{dx}\t{:.8}", dx.norm());

        if dx.norm() < EPS {
            break;
        }
    }

    Some(x)
}

fn main() {
    let f = Function::new(2, |v| {
        let (x, y) = (v[0], v[1]);

        let f1 = (x + 0.5).cos() + y - 1.;
        let f2 = y.sin() - 2. * x - 1.;

        Vector::new(vec![f1, f2])
    });

    let x0 = Vector::new(vec![-0.5, 0.0]);
    println!("Начальное приближение: {x0}");

    let x = newton(f, x0, 200).expect("error");
    println!("(~x, ~y) = {x}");
}
