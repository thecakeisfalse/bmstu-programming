use std::ops::{Index, IndexMut};

use crate::vector::Vector;

#[derive(Debug)]
pub struct Matrix {
    n: usize,
    data: Vec<Vec<f64>>,
}

impl Matrix {
    pub fn zero(n: usize) -> Self {
        Self {
            n,
            data: vec![vec![0.; n]; n],
        }
    }

    pub fn gauss(mut self, b: &Vector) -> Option<Vector> {
        assert_eq!(self.n, b.len());

        let n = self.n;
        let mut b = b.clone();

        for i in 0..n {
            let pivot = (i..n)
                .max_by(|&a, &b| self[(a, i)].abs().partial_cmp(&self[(b, i)].abs()).unwrap())?;

            if self[(pivot, i)].abs() < 1e-9 {
                return None;
            }

            if pivot != i {
                self.data.swap(pivot, i);
                b.swap(i, pivot);
            }

            for j in i + 1..n {
                let factor = self[(j, i)] / self[(i, i)];

                for k in i..n {
                    let val = self[(i, k)] * factor;
                    self.data[j][k] -= val;
                }

                b[j] -= b[i] * factor;
            }
        }

        let mut x = vec![0.0; n];
        for row in (0..n).rev() {
            let sum: f64 = (row + 1..n).map(|j| self[(row, j)] * x[j]).sum();
            x[row] = (b[row] - sum) / self[(row, row)];
        }

        Some(Vector::new(x))
    }
}

impl Index<(usize, usize)> for Matrix {
    type Output = f64;

    #[inline]
    fn index(&self, index: (usize, usize)) -> &Self::Output {
        assert!(index.0 < self.n && index.1 < self.n);

        &self.data[index.0][index.1]
    }
}

impl IndexMut<(usize, usize)> for Matrix {
    #[inline]
    fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
        assert!(index.0 < self.n && index.1 < self.n);

        &mut self.data[index.0][index.1]
    }
}
