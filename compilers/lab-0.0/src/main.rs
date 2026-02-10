#![allow(dead_code)]

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Define,
    End,
    If,
    EndIf,
    While,
    Do,
    Wend,
    Integer(i64),
    Word(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stop {
    End,
    EndIf,
    Do,
    Wend,
    Eof,
}

#[derive(Debug)]
enum Element {
    Word(String),
    Integer(i64),
    If { then_body: Body, cont: Body },
    While { cond: Body, body: Body, cont: Body },
}

type Body = Vec<Element>;
type Articles = HashMap<String, Body>;

#[derive(Debug)]
struct Program {
    articles: Articles,
    body: Body,
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    i: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, i: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.i)
    }

    fn next(&mut self) -> Option<Token> {
        let t = self.tokens.get(self.i).cloned();
        if t.is_some() {
            self.i += 1;
        }
        t
    }

    fn at_end(&self) -> bool {
        self.i >= self.tokens.len()
    }

    fn expect(&mut self, token: Token) -> Option<()> {
        match self.next() {
            Some(t) if t == token => Some(()),
            _ => None,
        }
    }

    // <Program> ::= <Articles> <Body> .
    fn parse_program(&mut self) -> Option<Program> {
        let articles = self.parse_articles()?;
        let (body, stop) = self.parse_body()?;

        if stop != Stop::Eof {
            return None;
        }

        Some(Program { articles, body })
    }

    // <Articles> ::= <Article> <Articles> | .
    fn parse_articles(&mut self) -> Option<Articles> {
        let mut map = Articles::new();

        while self.peek() == Some(&Token::Define) {
            let (name, body) = self.parse_article()?;
            if map.contains_key(&name) {
                return None;
            }
            map.insert(name, body);
        }

        Some(map)
    }

    // <Article> ::= define word <Body> end .
    fn parse_article(&mut self) -> Option<(String, Body)> {
        self.expect(Token::Define)?;

        let name = match self.next() {
            Some(Token::Word(w)) => w,
            _ => return None,
        };

        let (body, stop) = self.parse_body()?;
        if stop != Stop::End {
            return None;
        }
        self.next();

        Some((name, body))
    }

    // <Body> ::= if <Body> endif <Body>
    //        | while <Body> do <Body> wend <Body>
    //        | integer <Body>
    //        | word <Body>
    //        | .
    fn parse_body(&mut self) -> Option<(Body, Stop)> {
        let mut out = Vec::new();

        loop {
            if self.at_end() {
                return Some((out, Stop::Eof));
            }

            match self.peek()? {
                Token::End => return Some((out, Stop::End)),
                Token::EndIf => return Some((out, Stop::EndIf)),
                Token::Do => return Some((out, Stop::Do)),
                Token::Wend => return Some((out, Stop::Wend)),
                _ => {}
            }

            match self.next().unwrap() {
                Token::If => {
                    let (then_body, stop) = self.parse_body()?;
                    if stop != Stop::EndIf {
                        return None;
                    }
                    self.next();

                    let (cont, stop) = self.parse_body()?;
                    out.push(Element::If { then_body, cont });
                    return Some((out, stop));
                }

                Token::While => {
                    let (cond, stop) = self.parse_body()?;
                    if stop != Stop::Do {
                        return None;
                    }
                    self.next();

                    let (body, stop) = self.parse_body()?;
                    if stop != Stop::Wend {
                        return None;
                    }
                    self.next();

                    let (cont, stop) = self.parse_body()?;
                    out.push(Element::While { cond, body, cont });
                    return Some((out, stop));
                }

                Token::Integer(n) => out.push(Element::Integer(n)),
                Token::Word(w) => out.push(Element::Word(w)),

                _ => return None,
            }
        }
    }
}

fn lex(input: &str) -> Option<Vec<Token>> {
    let mut out = Vec::new();

    for raw in input.split_whitespace() {
        let t = match raw {
            "define" => Token::Define,
            "end" => Token::End,
            "if" => Token::If,
            "endif" => Token::EndIf,
            "while" => Token::While,
            "do" => Token::Do,
            "wend" => Token::Wend,
            _ if raw.chars().all(|c| c.is_ascii_digit()) => Token::Integer(raw.parse().ok()?),
            _ => Token::Word(raw.to_string()),
        };
        out.push(t);
    }

    Some(out)
}

fn parse(s: &str) -> Option<Program> {
    let tokens = lex(s)?;
    let mut p = Parser::new(tokens);
    p.parse_program()
}

fn main() {
    // let p = parse("1 2 + ");
    // let p = parse("x dup 0 swap if drop -1 endif");
    // let p = parse("1 x dup while dup 0 > do 1 - swap over * swap wend");
    let p = parse(
        "define -- 1 - end
         define =0? dup 0 = end
         define =1? dup 1 = end
         define factorial
             =0? if drop 1 exit endif
             =1? if drop 1 exit endif
             1 swap
             while dup 0 > do
                 1 - swap over * swap
             wend
             drop
         end
         0 factorial
         1 factorial
         2 factorial
         3 factorial
         4 factorial ",
    );
    println!("{p:?}");
}
