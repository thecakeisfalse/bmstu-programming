use regex::Regex;
use std::fmt;

#[derive(Clone, Copy)]
enum TokenKind {
    String,
    Binary,
    Number,
    Ident,
    Fraction,
}

impl fmt::Debug for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::String => write!(f, "STRING"),
            TokenKind::Binary => write!(f, "BINARY"),
            TokenKind::Number => write!(f, "NUMBER"),
            TokenKind::Ident => write!(f, "IDENT"),
            TokenKind::Fraction => write!(f, "FRACTION"),
        }
    }
}

#[derive(Debug)]
struct Rule {
    kind: TokenKind,
    re: Regex,
}

impl Rule {
    fn new<S: AsRef<str>>(pattern: S, kind: TokenKind) -> Self {
        Self {
            kind,
            re: Regex::new(pattern.as_ref()).expect("invalid regex"),
        }
    }
}

struct Token {
    kind: TokenKind,
    value: String,
    pos: (usize, usize),
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Token {
            kind,
            value,
            pos: (x, y),
        } = self;
        write!(f, "{kind:?} ({x}, {y}): {value}")
    }
}

struct TokenErr {
    pos: (usize, usize),
}

impl fmt::Debug for TokenErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "syntax error ({}, {})", self.pos.0, self.pos.1)
    }
}

struct TokenIter<'a> {
    lexer: &'a Lexer,
    input: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl TokenIter<'_> {
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
                Some(c) => c.to_string(),
                None => break,
            };

            self.skip(&ch);
            self.pos += ch.len();

            if self.pos >= self.input.len() || self.can_start_token() {
                break;
            }
        }
    }
}

impl Iterator for TokenIter<'_> {
    type Item = Result<Token, TokenErr>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.pos < self.input.len() && self.skip_whitespace() {}

        if self.pos >= self.input.len() {
            return None;
        }

        let mut best: Option<(TokenKind, usize)> = None;

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

struct Lexer {
    rules: Vec<Rule>,
    whitespace: Regex,
}

// Задача: добавить рациональную дробь вида ddd/dddd

impl Lexer {
    fn new() -> Self {
        Self {
            rules: vec![
                Rule::new(r"^`([^`]|``)*`", TokenKind::String),
                Rule::new(r"^[01]+b", TokenKind::Binary),
                Rule::new(r"^[0-9]+", TokenKind::Number),
                Rule::new(r"^[?*|][0-9?*|]*", TokenKind::Ident),
                Rule::new(r"^[0-9]+/[0-9]+", TokenKind::Fraction),
            ],
            whitespace: Regex::new(r"^[\t\n\r ]+").expect("invalid regex"),
        }
    }

    fn iter<'a>(&'a self, input: &'a str) -> impl Iterator<Item = Result<Token, TokenErr>> {
        TokenIter {
            lexer: self,
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }
}

fn main() {
    let filename = std::env::args().nth(1).expect("expected filename");
    let input = std::fs::read_to_string(&filename).expect("cannot read file");

    let lexer = Lexer::new();

    for result in lexer.iter(&input) {
        match result {
            Ok(tok) => println!("{tok:?}"),
            Err(err) => eprintln!("{err:?}"),
        }
    }
}
