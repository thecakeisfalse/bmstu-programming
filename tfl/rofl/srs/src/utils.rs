pub fn match_indices(s: &str, pattern: &str) -> Vec<usize> {
    let s: Vec<_> = format!("{pattern}#{s}").chars().collect();
    let mut p = vec![0_usize; s.len()];

    for i in 1..s.len() {
        let mut j = p[i - 1];

        while j > 0 && s[i] != s[j] {
            j = p[j - 1];
        }

        if s[i] == s[j] {
            j += 1;
        }

        p[i] = j;
    }

    p.into_iter()
        .enumerate()
        .filter(|(_, v)| *v == pattern.len())
        .map(|(i, _)| i - 2 * pattern.len())
        .collect()
}

#[cfg(test)]
mod test {
    use super::match_indices;

    #[test]
    fn doesnt_contain() {
        let s = "abasdfksdkjfasd";
        let t = "dd";

        assert!(match_indices(s, t).is_empty());
    }

    #[test]
    fn pattern_longer_than_string() {
        let s = "asdfg";
        let t = "asdfghewq";

        assert!(match_indices(s, t).is_empty());
    }

    #[test]
    fn one_match_begin() {
        let s = "aabbccddaad";
        let t = "aab";

        assert_eq!(match_indices(s, t), vec![0]);
    }

    #[test]
    fn one_match_end() {
        let s = "aabbccddaad";
        let t = "ad";

        assert_eq!(match_indices(s, t), vec![9]);
    }

    #[test]
    fn multiple_matches() {
        let s = "Hello, world!";
        let t = "o";

        assert_eq!(match_indices(s, t), vec![4, 8]);
    }

    #[test]
    fn overlap_matches() {
        let s = "stototosto";
        let t = "toto";

        assert_eq!(match_indices(s, t), vec![1, 3]);
    }
}
