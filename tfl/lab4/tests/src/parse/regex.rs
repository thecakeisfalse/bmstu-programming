use once_cell::sync::Lazy;
use pcre2::bytes::Regex;

use crate::TryParse;

static RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(a|b\1b)*c((?:a|b)*)((?:a|b)*)((?:a|b)*)c(?=\1$)(?=\2\3$)\3\4$")
        .expect("Failed to create regex")
});

pub struct RegexParser;

impl TryParse for RegexParser {
    fn try_parse<S: AsRef<[u8]> + ?Sized>(s: &S) -> Option<bool> {
        let m = RE.find(s.as_ref()).ok()?;
        Some(m.is_some())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple_tests() {
        assert_eq!(RegexParser::try_parse("cc"), Some(false));
        assert_eq!(RegexParser::try_parse("acaca"), Some(true));
        assert_eq!(RegexParser::try_parse("caca"), Some(false));
        assert_eq!(RegexParser::try_parse("ababbbabbacaca"), Some(true));
    }
}
