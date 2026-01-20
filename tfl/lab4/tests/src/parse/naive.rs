#![allow(clippy::too_many_arguments)]

use crate::Parse;

pub struct NaiveParser;

impl NaiveParser {
    fn is_valid(s: &[u8]) -> bool {
        #[derive(Debug, Default, Clone)]
        struct State {
            pointer: usize,
            num_c: usize,
            cur_ref: usize,
            ref1: String,
            ref2: String,
            ref3: String,
            ref4: String,
            tail: String,
            ref1_exp: String,
        }

        let mut stack = vec![State::default(); 1];

        while let Some(q) = stack.pop() {
            if q.pointer == s.len() {
                if q.ref1 == q.ref2 + &q.ref3 && q.ref3 + &q.ref4 == q.tail && q.ref1 == q.tail {
                    return true;
                }

                continue;
            }

            if q.num_c >= 4 && q.ref1 != q.ref2.clone() + &q.ref3 {
                continue;
            }

            if s[q.pointer] == b'c' {
                let mut new_ref1 = q.ref1;
                if q.num_c == 0 {
                    if q.ref1_exp.is_empty() {
                        new_ref1 = "b".to_string() + &new_ref1 + "b";
                    } else if q.ref1_exp.len() != new_ref1.len() + 2 || new_ref1.is_empty() {
                        continue;
                    }
                }

                stack.push(State {
                    pointer: q.pointer + 1,
                    num_c: q.num_c + 1,
                    cur_ref: q.cur_ref,
                    ref1: new_ref1,
                    ref2: q.ref2,
                    ref3: q.ref3,
                    ref4: q.ref4,
                    tail: q.tail,
                    ref1_exp: q.ref1_exp,
                });

                continue;
            }

            match q.num_c {
                0 => {
                    if q.ref1.is_empty() {
                        if s[q.pointer] != b'a' {
                            continue;
                        }

                        let ref1 = "a".to_string();
                        let ref1_exp = "bab".to_string();

                        stack.push(State {
                            pointer: q.pointer + 1,
                            num_c: q.num_c,
                            cur_ref: q.cur_ref,
                            ref1,
                            ref2: q.ref2,
                            ref3: q.ref3,
                            ref4: q.ref4,
                            tail: q.tail,
                            ref1_exp,
                        });
                    } else if q.ref1_exp.len() == 2 + q.ref1.len() && s[q.pointer] == b'a' {
                        let ref1 = "a".to_string();
                        let ref1_exp = "bab".to_string();

                        stack.push(State {
                            pointer: q.pointer + 1,
                            num_c: q.num_c,
                            cur_ref: q.cur_ref,
                            ref1,
                            ref2: q.ref2,
                            ref3: q.ref3,
                            ref4: q.ref4,
                            tail: q.tail,
                            ref1_exp,
                        });
                    } else if !q.ref1_exp.is_empty() {
                        if s[q.pointer] != q.ref1_exp.as_bytes()[q.ref1_exp.len() - 1] {
                            continue;
                        }

                        let mut new_ref1_exp = q.ref1_exp.clone();
                        new_ref1_exp.pop();

                        stack.push(State {
                            pointer: q.pointer + 1,
                            num_c: q.num_c,
                            cur_ref: q.cur_ref,
                            ref1: q.ref1,
                            ref2: q.ref2,
                            ref3: q.ref3,
                            ref4: q.ref4,
                            tail: q.tail,
                            ref1_exp: new_ref1_exp,
                        });
                    } else {
                        let ref1 = "b".to_string() + &q.ref1 + "b";
                        let ref1_exp = "b".to_string() + &ref1 + "b";

                        stack.push(State {
                            pointer: q.pointer,
                            num_c: q.num_c,
                            cur_ref: q.cur_ref,
                            ref1,
                            ref2: q.ref2,
                            ref3: q.ref3,
                            ref4: q.ref4,
                            tail: q.tail,
                            ref1_exp,
                        });
                    }
                }
                2 => {
                    let mut new_tail = q.tail.clone();
                    new_tail.push(s[q.pointer] as char);
                    stack.push(State {
                        pointer: q.pointer + 1,
                        num_c: q.num_c,
                        cur_ref: q.cur_ref,
                        ref1: q.ref1.clone(),
                        ref2: q.ref2.clone(),
                        ref3: q.ref3.clone(),
                        ref4: q.ref4.clone(),
                        tail: new_tail,
                        ref1_exp: q.ref1_exp.clone(),
                    });
                }
                1 => {
                    if q.cur_ref <= 2 {
                        let mut ref2 = q.ref2.clone();
                        ref2.push(s[q.pointer] as char);

                        stack.push(State {
                            pointer: q.pointer + 1,
                            num_c: q.num_c,
                            cur_ref: 2,
                            ref1: q.ref1.clone(),
                            ref2,
                            ref3: q.ref3.clone(),
                            ref4: q.ref4.clone(),
                            tail: q.tail.clone(),
                            ref1_exp: q.ref1_exp.clone(),
                        });
                    }

                    if q.cur_ref <= 3 {
                        let mut ref3 = q.ref3.clone();
                        ref3.push(s[q.pointer] as char);

                        stack.push(State {
                            pointer: q.pointer + 1,
                            num_c: q.num_c,
                            cur_ref: 3,
                            ref1: q.ref1.clone(),
                            ref2: q.ref2.clone(),
                            ref3,
                            ref4: q.ref4.clone(),
                            tail: q.tail.clone(),
                            ref1_exp: q.ref1_exp.clone(),
                        });
                    }

                    if q.cur_ref <= 4 {
                        let mut ref4 = q.ref4.clone();
                        ref4.push(s[q.pointer] as char);

                        stack.push(State {
                            pointer: q.pointer + 1,
                            num_c: q.num_c,
                            cur_ref: 4,
                            ref1: q.ref1.clone(),
                            ref2: q.ref2.clone(),
                            ref3: q.ref3.clone(),
                            ref4,
                            tail: q.tail.clone(),
                            ref1_exp: q.ref1_exp.clone(),
                        });
                    }
                }
                _ => continue,
            }
        }

        false
    }
}

impl Parse for NaiveParser {
    fn parse<S: AsRef<[u8]> + ?Sized>(s: &S) -> bool {
        Self::is_valid(s.as_ref())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_tests() {
        assert!(!NaiveParser::parse("cc"));
        assert!(NaiveParser::parse("acaca"));
        assert!(!NaiveParser::parse("caca"));
        assert!(NaiveParser::parse("ababbbabbacaca"));
        assert!(NaiveParser::parse("ababbbabbcbbabbabbcbbabb"));
    }
}
