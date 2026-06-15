use std::{
    collections::HashMap,
    env::args,
    fmt,
    fs::File,
    io::{self, Read},
    iter::{Fuse, Peekable},
};

#[derive(Clone, Copy)]
pub struct Position {
    row: usize,
    col: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.row, self.col)
    }
}

struct CharIter<R: Read>(R);

impl<R: Read> CharIter<R> {
    fn new(reader: R) -> Self {
        Self(reader)
    }
}

impl<R: Read> Iterator for CharIter<R> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut buf = [0u8; 4];
        self.0.read_exact(&mut buf[..1]).ok()?;

        let width = match buf[0] {
            b if b < 0x80 => 1,
            b if b < 0xE0 => 2,
            b if b < 0xF0 => 3,
            _ => 4,
        };

        if width > 1 {
            self.0.read_exact(&mut buf[1..width]).ok()?;
        }

        std::str::from_utf8(&buf[..width]).ok()?.chars().next()
    }
}

pub struct Segment {
    start: Position,
    end: Position,
}

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

pub enum Token {
    Integer { s: Segment, v: u64 },
    String { s: Segment, v: String },
    Identifier { s: Segment, v: usize },
    Eof { p: Position },
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer { s, v } => write!(f, "Integer {s}: {v}"),
            Self::Identifier { s, v } => write!(f, "Ident {s}: {v}"),
            Self::String { s, v } => write!(f, "String {s}: {v:?}"),
            Self::Eof { p } => write!(f, "Eof {p}"),
        }
    }
}

pub struct ScanError {
    message: String,
    p: Position,
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error {}: {}", self.p, self.message)
    }
}

pub struct Scanner<'a, R: Read> {
    p: Position,
    r: Peekable<Fuse<CharIter<R>>>,
    done: bool,
    table: HashMap<String, usize>,
    comments: &'a mut Vec<Segment>,
}

impl<'a, R: Read> Scanner<'a, R> {
    pub fn new(reader: R, comments: &'a mut Vec<Segment>) -> Self {
        Self {
            p: Position { row: 1, col: 1 },
            r: CharIter::new(reader).fuse().peekable(),
            done: false,
            table: HashMap::new(),
            comments,
        }
    }

    fn advance(&mut self) {
        let Some(cur) = self.r.next() else { return };

        let (row, col) = match cur {
            '\n' => (self.p.row + 1, 1),
            '\r' => {
                if matches!(self.r.peek(), Some('\n')) {
                    self.r.next();
                }
                (self.p.row + 1, 1)
            }
            _ => (self.p.row, self.p.col + 1),
        };

        self.p = Position { row, col };
    }

    fn skip_whitespace(&mut self) {
        while let Some(&cur) = self.r.peek()
            && cur.is_whitespace()
        {
            self.advance();
        }
    }

    fn parse_ident(&mut self) -> usize {
        let mut value = String::new();

        while let Some(&cur) = self.r.peek() {
            if !matches!(cur, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_') {
                break;
            }

            value.push(cur);
            self.advance();
        }

        let n = self.table.len();
        *self.table.entry(value).or_insert(n)
    }

    fn parse_str(&mut self) -> Option<String> {
        self.advance();

        let mut value = String::new();

        loop {
            let &cur = self.r.peek()?;

            match cur {
                '\\' => {
                    self.advance();

                    let escape = match self.r.peek()? {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        c => panic!("unknown escape character: {c}"),
                    };

                    value.push(escape);
                }
                '"' => {
                    self.advance();
                    break;
                }
                _ => value.push(cur),
            }

            self.advance();
        }

        Some(value)
    }

    fn parse_int(&mut self) -> Result<u64, ScanError> {
        let mut value = String::new();
        let start = self.p;

        while let Some(&cur) = self.r.peek()
            && (cur.is_numeric() || cur == '.')
        {
            value.push(cur);
            self.advance();
        }

        let mut groups = value.split('.');
        let first = groups.next().expect("empty string");

        if first.len() > 3 || groups.any(|g| g.len() != 3) {
            return Err(ScanError {
                message: format!("invalid number: {value}"),
                p: start,
            });
        }

        Ok(value.replace('.', "").parse().expect("u64 overflow"))
    }
}

impl<R: Read> Iterator for Scanner<'_, R> {
    type Item = Result<Token, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        loop {
            self.skip_whitespace();

            let Some(&cur) = self.r.peek() else {
                self.done = true;
                return Some(Ok(Token::Eof {
                    p: Position {
                        row: self.p.row,
                        col: self.p.col,
                    },
                }));
            };

            return Some(match cur {
                c if c.is_numeric() => {
                    let start = self.p;
                    self.parse_int().map(|v| {
                        let end = Position {
                            row: self.p.row,
                            col: self.p.col - 1,
                        };
                        Token::Integer {
                            s: Segment { start, end },
                            v,
                        }
                    })
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let start = self.p;
                    let v = self.parse_ident();
                    let end = Position {
                        row: self.p.row,
                        col: self.p.col - 1,
                    };
                    Ok(Token::Identifier {
                        s: Segment { start, end },
                        v,
                    })
                }
                '"' => {
                    let start = self.p;
                    let Some(v) = self.parse_str() else {
                        self.done = true;
                        return Some(Err(ScanError {
                            message: "Expected \", but EOF found".into(),
                            p: self.p,
                        }));
                    };
                    let end = Position {
                        row: self.p.row,
                        col: self.p.col - 1,
                    };
                    Ok(Token::String {
                        s: Segment { start, end },
                        v,
                    })
                }
                '$' => {
                    let start = self.p;
                    while !matches!(self.r.peek(), Some('\n' | '\r') | None) {
                        self.advance();
                    }

                    let end = Position {
                        row: self.p.row,
                        col: self.p.col - 1,
                    };
                    self.comments.push(Segment { start, end });
                    self.next()?
                }
                _ => {
                    let p = self.p;
                    self.advance();
                    Err(ScanError {
                        message: format!("unexpected symbol: {cur}"),
                        p,
                    })
                }
            });
        }
    }
}

fn main() -> io::Result<()> {
    let reader: Box<dyn Read> = if let Some(filename) = args().nth(1) {
        Box::new(File::open(filename)?)
    } else {
        Box::new(io::stdin())
    };

    let mut comments: Vec<Segment> = vec![];
    let scanner = Scanner::new(reader, &mut comments);

    for token in scanner {
        match token {
            Ok(token) => println!("{token}"),
            Err(err) => println!("{err}"),
        }
    }

    for s in comments {
        println!("{s}");
    }

    Ok(())
}

// Комментарии, начинаются на $ и прододлжаются до конца строки,
// токен не создаётся, фрагменты складываются в список комментариев (надо распечатать)
