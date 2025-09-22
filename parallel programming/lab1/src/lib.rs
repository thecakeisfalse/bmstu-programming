use std::fmt::Debug;
use std::marker::{Send, Sync};
use std::ops::{AddAssign, Deref, DerefMut, Mul};
use std::sync::{Arc, Mutex};
use std::thread;

pub trait Number<T>:
    Default + Copy + AddAssign + Mul<Output = T> + PartialEq + Sync + Send + Debug
{
}

impl<T> Number<T> for T where
    T: Default + Copy + AddAssign + Mul<Output = T> + PartialEq + Sync + Send + Debug
{
}

pub fn is_vec_slice_squared<T>(data: &[Vec<T>]) -> bool {
    data.iter().all(|v| v.len() == data.len())
}

const THREADS_COUNT: usize = 15;

#[derive(Default, Debug, PartialEq)]
pub struct Matrix<T: Number<T>>(Vec<Vec<T>>);

impl<T> Matrix<T>
where
    T: Number<T>,
{
    pub fn new(n: usize) -> Self {
        Self(vec![vec![T::default(); n]; n])
    }

    pub fn is_square_matrix(&self) -> bool {
        !self.is_empty() && is_vec_slice_squared(&self[..])
    }

    pub fn multiply_by_rows(&self, other: &Self) -> Self {
        assert!(self.is_square_matrix());
        assert!(other.is_square_matrix());
        assert_eq!(self.len(), other.len());

        let n = self.len();
        let mut ans = Self::new(n);

        for i in 0..n {
            for j in 0..n {
                for k in 0..n {
                    ans[i][j] += self[i][k] * other[k][j];
                }
            }
        }

        ans
    }

    pub fn multiply_by_cols(&self, other: &Self) -> Self {
        assert!(self.is_square_matrix());
        assert!(other.is_square_matrix());
        assert_eq!(self.len(), other.len());

        let n = self.len();
        let mut ans = Self::new(n);

        for j in 0..n {
            for i in 0..n {
                for k in 0..n {
                    ans[i][j] += self[i][k] * other[k][j];
                }
            }
        }

        ans
    }

    pub fn multiply_threads(&self, other: &Self) -> Self {
        assert!(self.is_square_matrix());
        assert!(other.is_square_matrix());
        assert_eq!(self.len(), other.len());

        let n = self.len();
        let ans = Arc::new(Mutex::new(Self::new(n)));

        let block_size = (n - 1 + THREADS_COUNT) / THREADS_COUNT;

        thread::scope(|s| {
            let handles: Vec<_> = (0..THREADS_COUNT)
                .map(|i| (i * block_size, n.min((i + 1) * block_size)))
                .map(|(start, end)| {
                    let ans = ans.clone();

                    s.spawn(move || {
                        for i in start..end {
                            for j in 0..n {
                                let mut sum = T::default();
                                for k in 0..n {
                                    sum += self[i][k] * other[k][j];
                                }
                                let mut ans = ans.lock().unwrap();
                                ans[i][j] += sum;
                            }
                        }
                    })
                })
                .collect();

            for handle in handles {
                handle.join().unwrap();
            }
        });

        Arc::try_unwrap(ans)
            .expect("Threads still have references")
            .into_inner()
            .expect("Mutex is poisoned")
    }

    pub fn multiply_unsafe(&self, other: &Self) -> Self {
        assert!(self.is_square_matrix());
        assert!(other.is_square_matrix());
        assert_eq!(self.len(), other.len());

        let n = self.len();

        let mut other_transpose = Self::new(n);

        for i in 0..n {
            for j in 0..n {
                unsafe {
                    *other_transpose.get_unchecked_mut(i).get_unchecked_mut(j) =
                        *other.get_unchecked(j).get_unchecked(i);
                }
            }
        }

        let mut ans = Self::new(n);

        for i in 0..n {
            for j in 0..n {
                let mut sum = T::default();

                for k in 0..n {
                    unsafe {
                        sum += *self.get_unchecked(i).get_unchecked(k)
                            * *other_transpose.get_unchecked(j).get_unchecked(k);
                    }
                }

                unsafe {
                    *ans.get_unchecked_mut(i).get_unchecked_mut(j) = sum;
                }
            }
        }

        ans
    }

    pub fn multiply_unsafe_threads(&self, other: &Self) -> Self {
        assert!(self.is_square_matrix());
        assert!(other.is_square_matrix());
        assert_eq!(self.len(), other.len());

        let n = self.len();
        let ans = Arc::new(Mutex::new(Self::new(n)));

        let block_size = (n - 1 + THREADS_COUNT) / THREADS_COUNT;

        let mut other_transpose = Self::new(n);

        for i in 0..n {
            for j in 0..n {
                unsafe {
                    *other_transpose.get_unchecked_mut(i).get_unchecked_mut(j) =
                        *other.get_unchecked(j).get_unchecked(i);
                }
            }
        }

        let other_transpose = Arc::from(other_transpose);

        thread::scope(|s| {
            let handles: Vec<_> = (0..THREADS_COUNT)
                .map(|i| (i * block_size, n.min((i + 1) * block_size)))
                .map(|(start, end)| {
                    let ans = ans.clone();
                    let other_transpose = other_transpose.clone();

                    s.spawn(move || {
                        for i in start..end {
                            for j in 0..n {
                                let mut sum = T::default();
                                for k in 0..n {
                                    unsafe {
                                        sum += *self.get_unchecked(i).get_unchecked(k)
                                            * *other_transpose.get_unchecked(j).get_unchecked(k);
                                    }
                                }
                                let mut ans = ans.lock().unwrap();
                                unsafe {
                                    *ans.get_unchecked_mut(i).get_unchecked_mut(j) = sum;
                                }
                            }
                        }
                    })
                })
                .collect();

            handles
                .into_iter()
                .for_each(|handle| handle.join().unwrap());
        });

        Arc::try_unwrap(ans)
            .expect("Threads still have references")
            .into_inner()
            .expect("Mutex is poisoned")
    }
}

impl<T: Number<T>> Deref for Matrix<T> {
    type Target = Vec<Vec<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Number<T>> DerefMut for Matrix<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Number<T>> From<Vec<Vec<T>>> for Matrix<T> {
    fn from(data: Vec<Vec<T>>) -> Self {
        Self(data)
    }
}

#[cfg(test)]
mod test {
    use super::{Matrix, is_vec_slice_squared};

    use proptest::collection::vec;
    use proptest::prelude::*;

    const MAX: i32 = 10;
    const N: usize = 10;

    prop_compose! {
        fn arb_matrix(size: usize)
            (data in vec(vec(0..=MAX, size..=size), size..=size)
                .prop_filter("Must be square matrix", |data| {
                    is_vec_slice_squared(&data[..])
                })) -> Matrix<i32> {
            data.into()
        }
    }

    proptest! {
        #[test]
        fn prop_product_test(a in arb_matrix(N), b in arb_matrix(N)) {
            prop_assume!(a.len() == b.len());

            let c_1 = a.multiply_by_rows(&b);
            let c_2 = a.multiply_by_cols(&b);
            let c_3 = a.multiply_threads(&b);
            let c_4 = a.multiply_unsafe(&b);
            let c_5 = a.multiply_unsafe_threads(&b);

            assert_eq!(c_1, c_2);
            assert_eq!(c_1, c_3);
            assert_eq!(c_1, c_4);
            assert_eq!(c_1, c_5);
        }

        #[test]
        fn prop_product_test2(a in arb_matrix(N)) {
            let c_1 = a.multiply_by_rows(&a);
            let c_2 = a.multiply_by_cols(&a);
            let c_3 = a.multiply_threads(&a);
            let c_4 = a.multiply_unsafe(&a);
            let c_5 = a.multiply_unsafe_threads(&a);

            assert_eq!(c_1, c_2);
            assert_eq!(c_1, c_3);
            assert_eq!(c_1, c_4);
            assert_eq!(c_1, c_5);
        }
    }

    #[test]
    fn simple_product_test1() {
        let a: Matrix<i32> = vec![
            vec![2, 9, 8],  //
            vec![5, 9, 1],  //
            vec![6, 3, 10], //
        ]
        .into();

        let b: Matrix<i32> = vec![
            vec![4, 1, 1],  //
            vec![8, 10, 9], //
            vec![9, 10, 2], //
        ]
        .into();

        let c_1 = a.multiply_by_rows(&b);
        let c_2 = a.multiply_by_cols(&b);
        let c_3 = a.multiply_threads(&b);
        let c_4 = a.multiply_unsafe(&b);
        let c_5 = a.multiply_unsafe_threads(&b);

        let ans = vec![
            vec![152, 172, 99], //
            vec![101, 105, 88], //
            vec![138, 136, 53], //
        ]
        .into();

        assert_eq!(c_1, c_2);
        assert_eq!(c_1, c_3);
        assert_eq!(c_1, c_4);
        assert_eq!(c_1, c_5);
        assert_eq!(c_1, ans);
    }

    #[test]
    fn simple_product_test2() {
        let a: Matrix<i32> = vec![
            vec![2, 9, 8], //
            vec![5, 9, 1], //
            vec![6, 3, 2], //
        ]
        .into();

        let c_1 = a.multiply_by_rows(&a);
        let c_2 = a.multiply_by_cols(&a);
        let c_3 = a.multiply_threads(&a);
        let c_4 = a.multiply_unsafe(&a);
        let c_5 = a.multiply_unsafe_threads(&a);

        let ans = vec![
            vec![97, 123, 41], //
            vec![61, 129, 51], //
            vec![39, 87, 55],  //
        ]
        .into();

        assert_eq!(c_1, c_2);
        assert_eq!(c_1, c_3);
        assert_eq!(c_1, c_4);
        assert_eq!(c_1, c_5);
        assert_eq!(c_1, ans);
    }

    #[test]
    fn random_product_test() {
        use rand::random_range as rng;

        const TEST_COUNTS: usize = 1000;

        for _ in 1..=TEST_COUNTS {
            let n = rng(10..50);

            let a: Matrix<i32> = rand_matrix_i32(n);
            let b: Matrix<i32> = rand_matrix_i32(n);

            let c_1 = a.multiply_by_rows(&b);
            let c_2 = a.multiply_by_cols(&b);
            let c_3 = a.multiply_threads(&b);
            let c_4 = a.multiply_unsafe(&b);
            let c_5 = a.multiply_unsafe_threads(&b);

            assert_eq!(c_1, c_2, "testing for {a:#?} {b:#?}.");
            assert_eq!(c_1, c_3, "testing for {a:#?} {b:#?}.");
            assert_eq!(c_1, c_4, "testing for {a:#?} {b:#?}.");
            assert_eq!(c_1, c_5, "testing for {a:#?} {b:#?}.");
        }
    }

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
}
