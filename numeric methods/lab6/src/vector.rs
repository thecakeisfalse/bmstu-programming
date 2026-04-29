use std::fmt;
use std::ops::{Add, AddAssign, Div, Index, IndexMut, Neg, Sub, SubAssign};

#[derive(Clone)]
pub struct Vector {
    data: Vec<f64>,
}

impl fmt::Debug for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

impl fmt::Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let data: Vec<_> = self.data.iter().map(|x| format!("{x:.8}")).collect();
        let data = data.join(", ");
        write!(f, "({data})")
    }
}

macro_rules! impl_op {
    ($struct:ident, $trait:ident, $method:ident) => {
        impl $trait for $struct {
            type Output = $struct;

            fn $method(self, rhs: $struct) -> $struct {
                (&self).$method(&rhs)
            }
        }

        impl $trait<&$struct> for $struct {
            type Output = $struct;

            fn $method(self, rhs: &$struct) -> $struct {
                (&self).$method(rhs)
            }
        }

        impl $trait<$struct> for &$struct {
            type Output = $struct;

            fn $method(self, rhs: $struct) -> $struct {
                self.$method(&rhs)
            }
        }
    };
}

macro_rules! impl_assign_op {
    ($struct:ident, $trait:ident, $method:ident) => {
        impl $trait for $struct {
            fn $method(&mut self, rhs: $struct) {
                self.$method(&rhs)
            }
        }
    };
}

impl Vector {
    pub fn new(data: Vec<f64>) -> Self {
        Self { data }
    }

    pub fn norm(&self) -> f64 {
        self.data
            .iter()
            .map(|x| x.abs())
            .fold(f64::NEG_INFINITY, f64::max)
    }

    pub fn norm2(&self) -> f64 {
        self.data.iter().map(|x| x.powi(2)).sum()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn swap(&mut self, i: usize, j: usize) {
        self.data.swap(i, j);
    }
}

impl Index<usize> for Vector {
    type Output = f64;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.data.len());

        &self.data[index]
    }
}

impl IndexMut<usize> for Vector {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.data.len());

        &mut self.data[index]
    }
}

impl Add for &Vector {
    type Output = Vector;

    fn add(self, rhs: &Vector) -> Vector {
        assert_eq!(self.data.len(), rhs.data.len());
        Vector::new(
            self.data
                .iter()
                .zip(rhs.data.iter())
                .map(|(x, y)| x + y)
                .collect(),
        )
    }
}

impl_op!(Vector, Add, add);

impl Sub for &Vector {
    type Output = Vector;

    fn sub(self, rhs: Self) -> Self::Output {
        Vector::new(
            self.data
                .iter()
                .zip(rhs.data.iter())
                .map(|(x, y)| x - y)
                .collect(),
        )
    }
}

impl_op!(Vector, Sub, sub);

impl Div<f64> for Vector {
    type Output = Vector;

    fn div(self, rhs: f64) -> Self::Output {
        Self::new(self.data.into_iter().map(|x| x / rhs).collect())
    }
}

impl AddAssign<&Vector> for Vector {
    fn add_assign(&mut self, rhs: &Vector) {
        assert_eq!(self.data.len(), rhs.data.len());

        for (x, y) in self.data.iter_mut().zip(rhs.data.iter()) {
            *x += y;
        }
    }
}

impl SubAssign<&Vector> for Vector {
    fn sub_assign(&mut self, rhs: &Vector) {
        assert_eq!(self.data.len(), rhs.data.len());

        for (x, y) in self.data.iter_mut().zip(rhs.data.iter()) {
            *x -= y;
        }
    }
}

impl_assign_op!(Vector, AddAssign, add_assign);
impl_assign_op!(Vector, SubAssign, sub_assign);

impl Neg for Vector {
    type Output = Vector;
    fn neg(mut self) -> Vector {
        self.data.iter_mut().for_each(|x| *x = -*x);
        self
    }
}
