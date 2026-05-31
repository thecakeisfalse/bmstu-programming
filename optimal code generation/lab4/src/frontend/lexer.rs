use std::{error::Error, iter::Peekable, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    IntLiteral,
    Ident,
    KwInt,
    KwIf,
    KwElse,
    KwFor,
    KwReturn,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Assign,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
}

#[derive(Debug)]
pub struct LexError {
    pub message: String,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LexError: {}", self.message)
    }
}

impl Error for LexError {}

struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    done: bool,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            done: false,
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.chars.peek(), Some(c) if c.is_ascii_whitespace()) {
            self.chars.next();
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.chars.peek() == Some(&expected) {
            self.chars.next();
            true
        } else {
            false
        }
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace();

        let c = match self.chars.peek().copied() {
            None => {
                return Ok(Token {
                    kind: TokenKind::Eof,
                    lexeme: "".into(),
                });
            }
            Some(c) => {
                self.chars.next();
                c
            }
        };

        let mut lexeme = c.to_string();

        if c.is_ascii_digit() {
            while matches!(self.chars.peek(), Some(c) if c.is_ascii_digit()) {
                lexeme.push(self.chars.next().unwrap());
            }
            return Ok(Token {
                kind: TokenKind::IntLiteral,
                lexeme,
            });
        }

        if c.is_ascii_alphabetic() || c == '_' {
            while matches!(self.chars.peek(), Some(c) if c.is_ascii_alphanumeric() || *c == '_') {
                lexeme.push(self.chars.next().unwrap());
            }
            let kind = match lexeme.as_str() {
                "int" => TokenKind::KwInt,
                "if" => TokenKind::KwIf,
                "else" => TokenKind::KwElse,
                "for" => TokenKind::KwFor,
                "return" => TokenKind::KwReturn,
                _ => TokenKind::Ident,
            };

            return Ok(Token { kind, lexeme });
        }

        let kind = match c {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => {
                if self.chars.peek() == Some(&'/') {
                    while !matches!(self.chars.peek(), None | Some('\n')) {
                        self.chars.next();
                    }
                    return self.next_token();
                }
                TokenKind::Slash
            }
            '%' => TokenKind::Percent,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            ';' => TokenKind::Semicolon,
            '=' => {
                if self.match_char('=') {
                    lexeme.push('=');
                    TokenKind::Eq
                } else {
                    TokenKind::Assign
                }
            }
            '<' => {
                if self.match_char('=') {
                    lexeme.push('=');
                    TokenKind::LtEq
                } else {
                    TokenKind::Lt
                }
            }
            '>' => {
                if self.match_char('=') {
                    lexeme.push('=');
                    TokenKind::GtEq
                } else {
                    TokenKind::Gt
                }
            }
            '!' => {
                if self.match_char('=') {
                    lexeme.push('=');
                    TokenKind::NotEq
                } else {
                    return Err(LexError {
                        message: format!("unexpected character '{c}'"),
                    });
                }
            }
            _ => {
                return Err(LexError {
                    message: format!("unexpected character '{c}'"),
                });
            }
        };

        Ok(Token { kind, lexeme })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        match self.next_token() {
            Ok(tok) if tok.kind == TokenKind::Eof => {
                self.done = true;
                Some(Ok(tok))
            }
            other => Some(other),
        }
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(source).collect()
}
