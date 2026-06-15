use regex::Regex;
use std::fmt;

#[derive(Debug)]
pub struct Rule<K: Clone + Copy> {
    kind: K,
    re: Regex,
}

impl<K: Clone + Copy> Rule<K> {
    pub fn new<S: AsRef<str>>(pattern: S, kind: K) -> Self {
        Self {
            kind,
            re: Regex::new(pattern.as_ref()).expect("invalid regex"),
        }
    }
}

#[derive(Clone)]
pub struct Token<K> {
    pub kind: K,
    pub value: String,
    pub pos: (usize, usize),
}

impl<K: std::fmt::Debug> fmt::Debug for Token<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Token {
            kind,
            value,
            pos: (x, y),
        } = self;
        write!(f, "{kind:?} ({x}, {y}): {value}")
    }
}

pub struct TokenErr {
    pos: (usize, usize),
}

impl fmt::Debug for TokenErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "syntax error ({}, {})", self.pos.0, self.pos.1)
    }
}

pub struct TokenIter<'a, K: Clone + Copy> {
    lexer: &'a Lexer<K>,
    input: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<K: Clone + Copy> TokenIter<'_, K> {
    fn skip(&mut self, text: &str) {
        for ch in text.chars() {
            if ch == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
    }

    fn skip_whitespace(&mut self) -> bool {
        if let Some(m) = self.lexer.whitespace.find(&self.input[self.pos..]) {
            self.skip(m.as_str());
            self.pos += m.len();
            true
        } else {
            false
        }
    }

    fn can_start_token(&self) -> bool {
        let rest = &self.input[self.pos..];
        self.lexer.whitespace.is_match(&rest)
            || self.lexer.rules.iter().any(|r| r.re.is_match(&rest))
    }

    fn skip_error(&mut self) {
        loop {
            let ch = match &self.input[self.pos..].chars().next() {
                Some(c) => *c,
                None => break,
            };

            let ch_str = ch.to_string();
            self.skip(&ch_str);
            self.pos += ch.len_utf8();

            if self.pos >= self.input.len() || self.can_start_token() {
                break;
            }
        }
    }
}

impl<K: Clone + Copy> Iterator for TokenIter<'_, K> {
    type Item = Result<Token<K>, TokenErr>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.pos < self.input.len() && self.skip_whitespace() {}

        if self.pos >= self.input.len() {
            return None;
        }

        let mut best: Option<(K, usize)> = None;

        for rule in &self.lexer.rules {
            if let Some(m) = rule.re.find(&self.input[self.pos..]) {
                if best.is_none() || m.len() > best.unwrap().1 {
                    best = Some((rule.kind, m.len()));
                }
            }
        }

        let (line, col) = (self.line, self.col);

        if let Some((kind, len)) = best {
            let value = self.input[self.pos..self.pos + len].to_string();
            self.skip(&value);
            self.pos += len;
            Some(Ok(Token {
                kind,
                value,
                pos: (line, col),
            }))
        } else {
            self.skip_error();
            Some(Err(TokenErr { pos: (line, col) }))
        }
    }
}

pub struct Lexer<K: Clone + Copy> {
    rules: Vec<Rule<K>>,
    whitespace: Regex,
}

impl<K: Clone + Copy> Lexer<K> {
    pub fn new(rules: Vec<Rule<K>>) -> Self {
        Self {
            rules,
            whitespace: Regex::new(r"^\s+").expect("invalid regex"),
        }
    }

    pub fn iter<'a>(&'a self, input: &'a str) -> impl Iterator<Item = Result<Token<K>, TokenErr>> {
        TokenIter {
            lexer: self,
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }
}
