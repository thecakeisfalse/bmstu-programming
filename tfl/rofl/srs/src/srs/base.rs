use rand::random_range as rng;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::mem::swap;
use std::ops::{Deref, DerefMut, Range};

use crate::utils::match_indices;

pub type Rule = (String, String);

#[derive(Default, Clone, Debug, PartialEq)]
pub struct StringRewritingSystem(Vec<Rule>);

// God, save your souls from skill issue
impl StringRewritingSystem {
    pub fn add_rule<S: Into<String>>(&mut self, lhs: S, rhs: S) {
        self.push((lhs.into(), rhs.into()));
    }

    pub fn rules(&self) -> &Vec<Rule> {
        &self.0
    }

    pub fn rewrite<S: Into<String>>(&self, s: S) -> impl Iterator<Item = String> {
        let s: String = s.into();

        self.iter().flat_map(move |(l, r)| {
            match_indices(&s, l)
                .into_iter()
                .map(|i| s[0..i].to_owned() + r + &s[(i + l.len())..])
                .collect::<Vec<_>>()
        })
    }

    pub fn rewrite_without<S: Into<String>>(
        &self,
        s: S,
        forbid: &str,
    ) -> impl Iterator<Item = String> {
        let s: String = s.into();

        self.iter()
            .filter(move |(l, _)| l != forbid)
            .flat_map(move |(l, r)| {
                match_indices(&s, l)
                    .into_iter()
                    .map(|i| s[0..i].to_owned() + r + &s[(i + l.len())..])
                    .collect::<Vec<_>>()
            })
    }

    pub fn kth_rewrite<S: Into<String>>(&self, s: S, k: usize) -> impl Iterator<Item = String> {
        let mut current = vec![s.into()];

        for _ in 0..k {
            current = current.into_iter().flat_map(|s| self.rewrite(s)).collect();
        }

        current.into_iter()
    }

    pub fn reorder<F>(&mut self, order: F)
    where
        F: Fn(&str, &str) -> Ordering,
    {
        for (l, r) in &mut self.0 {
            match order(l, r) {
                Ordering::Equal if l != r => panic!("invalid order"),
                Ordering::Less => swap(l, r),
                _ => (),
            }
        }
    }

    pub fn alphabet(&self) -> impl Iterator<Item = char> {
        HashSet::<_>::from_iter(self.0.iter().flat_map(|(lhs, _)| lhs.chars())).into_iter()
    }

    pub fn random_word(&self, length: Range<usize>) -> String {
        let alphabet = self.alphabet().collect::<Vec<_>>();

        (0..rng(length))
            .map(|_| alphabet[rng(0..alphabet.len())])
            .collect()
    }
}

impl Deref for StringRewritingSystem {
    type Target = Vec<Rule>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for StringRewritingSystem {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<S, I> From<I> for StringRewritingSystem
where
    S: Into<String>,
    I: IntoIterator<Item = (S, S)>,
{
    fn from(value: I) -> Self {
        let mut result = Self::default();

        value
            .into_iter()
            .for_each(|(lhs, rhs)| result.add_rule(lhs, rhs));

        result
    }
}

impl std::fmt::Display for StringRewritingSystem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = vec![];

        for (l, r) in self.iter() {
            buf.push(format!("{l} -> {r}"));
        }

        write!(f, "{}", buf.join("\n"))
    }
}

#[cfg(test)]
mod test {
    use super::StringRewritingSystem;
    use std::cmp::Ordering;

    fn lexic(t1: &str, t2: &str) -> Ordering {
        match t1.len().cmp(&t2.len()) {
            Ordering::Equal => t1.cmp(t2),
            x => x,
        }
    }

    #[test]
    fn rewrite() {
        let srs = StringRewritingSystem::from([
            ("bb", "a"), //
            ("aba", "b"),
            ("aa", "c"),
        ]);

        assert_eq!(
            srs.rewrite("bbb").collect::<Vec<_>>(), //
            vec!["ab", "ba"]
        );

        assert_eq!(
            srs.rewrite("ababb").collect::<Vec<_>>(), //
            vec!["abaa", "bbb"]
        );

        assert!(srs.kth_rewrite("bbb", 3).collect::<Vec<_>>().is_empty());

        // (1) ababb -> [abaa, bbb]
        // (2) abaa  -> [ba, abc]
        //     bbb   -> [ab, ba]

        assert_eq!(
            srs.kth_rewrite("ababb", 2).collect::<Vec<_>>(), //
            vec!["ba", "abc", "ab", "ba"]
        );

        assert!(srs.kth_rewrite("ababb", 3).collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn reorder() {
        let mut srs = StringRewritingSystem::from([
            ("aaa", "b"), //
            ("bb", "aaaa"),
            ("cda", "cdb"),
        ]);

        srs.reorder(lexic);

        let expected = StringRewritingSystem::from([
            ("aaa", "b"), //
            ("aaaa", "bb"),
            ("cdb", "cda"),
        ]);

        assert_eq!(srs, expected);
    }
}
