use crate::Parse;

pub struct OptimizedParser;

impl OptimizedParser {
    fn match_left_block(s: &[u8]) -> Option<(String, usize)> {
        let mut pointer = 0;
        let mut ref1 = 0;
        let mut exp_ref1 = 0;

        if s.is_empty() {
            return None;
        }

        while pointer < s.len() {
            match s[pointer] {
                b'a' => {
                    ref1 = 0;
                    exp_ref1 = 1;
                    pointer += 1;
                }
                b'b' if exp_ref1 > 0 => {
                    if pointer + 2 * exp_ref1 + 1 > s.len() {
                        return None;
                    }

                    let block = s[pointer..].iter().take(2 * exp_ref1 + 1);

                    let mut a = (0..)
                        .zip(block)
                        .filter(|(_, y)| **y == b'a')
                        .map(|(x, _)| x);

                    if a.next() != Some(exp_ref1) || a.next().is_some() {
                        return None;
                    }

                    pointer += 2 * exp_ref1 + 1;
                    exp_ref1 += 1;
                    ref1 += 1;
                }
                _ => return None,
            }
        }

        let ref1_s = "b".repeat(ref1) + "a" + &"b".repeat(ref1);

        Some((ref1_s, ref1))
    }

    fn match_middle_block(s: &[u8], ref1_size: usize) -> bool {
        let count_a = s.iter().filter(|x| **x == b'a').count();

        match count_a {
            1 => {
                s.iter()
                    .enumerate()
                    .filter(|(_, x)| **x == b'a')
                    .all(|(i, _)| i == ref1_size)
                    && s.len() == 2 * ref1_size + 1
            }
            2 => s.split(|x| *x == b'a').enumerate().all(|(i, x)| {
                if i % 2 == 0 {
                    x.len() == ref1_size
                } else {
                    ref1_size <= x.len() && x.len() <= 2 * ref1_size
                }
            }),
            _ => false,
        }
    }

    fn is_valid(s: &[u8]) -> bool {
        if s.iter().filter(|x| **x == b'c').count() != 2 {
            return false;
        }

        let blocks: Vec<_> = s.split(|x| *x == b'c').collect();

        let (ref1, n) = match Self::match_left_block(blocks[0]) {
            Some(v) => v,
            None => return false,
        };

        blocks[2] == ref1.as_bytes() && Self::match_middle_block(blocks[1], n)
    }
}

impl Parse for OptimizedParser {
    fn parse<S: AsRef<[u8]> + ?Sized>(s: &S) -> bool {
        Self::is_valid(s.as_ref())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_tests() {
        assert!(!OptimizedParser::parse("cc"));
        assert!(OptimizedParser::parse("acaca"));
        assert!(!OptimizedParser::parse("caca"));
        assert!(OptimizedParser::parse("ababbbabbacaca"));
    }
}
