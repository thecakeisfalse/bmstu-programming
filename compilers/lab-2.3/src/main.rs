#[allow(unused)]
mod lexer;

use lexer::{Lexer, Rule, Token};

use std::fmt;
use std::fs::File;
use std::io::Read;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    KwTokens,
    Ident,
    KwIs,
    KwStart,
    Dot,
    Comma,
    Comment,
    Eof,
}

#[derive(Clone, Copy, Debug)]
pub enum TermKind {
    Grammar,
    Tokens,
    Token,
    TokenTail,
    Rules,
    Rule,
    RuleTail,
    IdentList,
    Start,
}

impl TermKind {
    fn name(self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Clone, Copy)]
pub enum Sym {
    T(TokenKind),
    N(TermKind),
}

fn transition(nt: TermKind, tok: TokenKind) -> Option<Vec<Sym>> {
    use Sym::*;
    use TermKind::*;
    use TokenKind::*;

    Some(match (nt, tok) {
        (Grammar, KwTokens | Ident | KwStart) => vec![N(Tokens), N(Rules), N(Start)],
        (Tokens, KwTokens) => vec![N(Token), N(Tokens)],
        (Tokens, Ident | KwStart) => vec![],
        (Token, KwTokens) => vec![T(KwTokens), T(Ident), N(TokenTail), T(Dot)],
        (TokenTail, Comma) => vec![T(Comma), T(Ident), N(TokenTail)],
        (TokenTail, Dot) => vec![],
        (Rules, Ident) => vec![N(Rule), N(Rules)],
        (Rules, KwStart) => vec![],
        (Rule, Ident) => vec![T(Ident), T(KwIs), N(IdentList), N(RuleTail), T(Dot)],
        (IdentList, Ident) => vec![T(Ident), N(IdentList)],
        (IdentList, Comma | Dot) => vec![],
        (RuleTail, Comma) => vec![T(Comma), T(Ident), T(KwIs), N(IdentList), N(RuleTail)],
        (RuleTail, Dot) => vec![],
        (Start, KwStart) => vec![T(KwStart), T(Ident), T(Dot)],
        _ => return None,
    })
}

pub enum ParseTree {
    Inner(String, Vec<ParseTree>),
    Leaf(Option<Token<TokenKind>>),
}

impl ParseTree {
    pub fn to_dot(&self) -> String {
        let mut out = String::from("digraph {\n");
        let mut id = 0;
        self.dot_node(&mut out, &mut id);
        out.push_str("}\n");
        out
    }

    fn dot_node(&self, out: &mut String, counter: &mut usize) -> usize {
        let id = *counter;
        *counter += 1;

        let (name, children) = match self {
            ParseTree::Inner(name, children) => (name, children),
            ParseTree::Leaf(None) => (&format!("epsilon"), &vec![]),
            ParseTree::Leaf(Some(s)) => (&format!("{:?}: {}", s.kind, s.value), &vec![]),
        };

        let ids: Vec<_> = children.iter().map(|c| c.dot_node(out, counter)).collect();

        out.push_str(&format!("  n{id} [label = \"{name}\"]\n"));

        for &to_id in &ids {
            out.push_str(&format!("  n{id} -> n{to_id}\n"));
        }

        if ids.len() > 1 {
            let chain: Vec<String> = ids.iter().map(|i| format!("n{i}")).collect();
            out.push_str(&format!(
                "  {{ rank=same; {} [style=invis] }}\n",
                chain.join(" -> ")
            ));
        }

        id
    }
}

pub struct ParseErr {
    msg: String,
    pos: Option<(usize, usize)>,
}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error")?;

        if let Some(pos) = self.pos {
            write!(f, " ({}, {})", pos.0, pos.1)?;
        }

        write!(f, "{}", self.msg)
    }
}

fn top_down_parse(tokens: &Vec<Token<TokenKind>>) -> Result<Vec<(TermKind, Vec<Sym>)>, ParseErr> {
    let cur_kind = |i: usize| tokens.get(i).map(|t| t.kind).unwrap_or(TokenKind::Eof);

    let mut rules: Vec<(TermKind, Vec<Sym>)> = vec![];
    let mut stack: Vec<Sym> = vec![Sym::N(TermKind::Grammar)];

    let mut i = 0;

    loop {
        let x = match stack.last() {
            Some(s) => s.clone(),
            None => break,
        };

        match x {
            Sym::T(expected) => {
                if cur_kind(i) != expected {
                    let t = tokens.get(i);
                    return Err(ParseErr {
                        msg: format!("expected {expected:?}, got {:?}", cur_kind(i)),
                        pos: t.map(|t| t.pos),
                    });
                }

                stack.pop();
                i += 1;
            }
            Sym::N(n) => match transition(n, cur_kind(i)) {
                Some(next) => {
                    stack.pop();
                    for &sym in next.iter().rev() {
                        stack.push(sym);
                    }
                    rules.push((n, next));
                }
                None => {
                    let t = tokens.get(i);
                    return Err(ParseErr {
                        msg: format!("unexpected {:?}", cur_kind(i)),
                        pos: t.map(|t| t.pos),
                    });
                }
            },
        }
    }

    if i < tokens.len() {
        return Err(ParseErr {
            msg: format!("expected end of file, got {:?}", tokens[i].kind),
            pos: Some(tokens[i].pos),
        });
    }

    Ok(rules)
}

fn rules_to_tree(
    root: Sym,
    rules: &mut impl Iterator<Item = (TermKind, Vec<Sym>)>,
    tokens: &mut impl Iterator<Item = Token<TokenKind>>,
) -> ParseTree {
    match root {
        Sym::N(n) => {
            let (_, rhs) = rules.next().unwrap();

            if rhs.is_empty() {
                ParseTree::Inner(n.name(), vec![ParseTree::Leaf(None)])
            } else {
                let children = rhs
                    .into_iter()
                    .map(|s| rules_to_tree(s, rules, tokens))
                    .collect();
                ParseTree::Inner(n.name(), children)
            }
        }
        Sym::T(_) => {
            let token = tokens.next().unwrap();
            ParseTree::Leaf(Some(token))
        }
    }
}

fn parse(tokens: Vec<Token<TokenKind>>) -> Result<ParseTree, ParseErr> {
    let tokens: Vec<_> = tokens
        .into_iter()
        .filter(|x| x.kind != TokenKind::Comment)
        .collect();

    let rules = top_down_parse(&tokens)?;

    let tree = rules_to_tree(
        Sym::N(TermKind::Grammar),
        &mut rules.into_iter(),
        &mut tokens.into_iter(),
    );

    Ok(tree)
}

fn main() -> std::io::Result<()> {
    let lexer = Lexer::new(vec![
        Rule::new("^\\(\\*[^()]*\\*\\)", TokenKind::Comment),
        Rule::new("^\\([^()]*\\)", TokenKind::Ident), //
        Rule::new("^tokens", TokenKind::KwTokens),    //
        Rule::new("^is", TokenKind::KwIs),            //
        Rule::new("^start", TokenKind::KwStart),      //
        Rule::new(r"^\.", TokenKind::Dot),            //
        Rule::new(r"^,", TokenKind::Comma),           //
    ]);

    let mut file = File::open("in.txt")?;

    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let tokens: Vec<_> = lexer.iter(&buf).filter_map(|x| x.ok()).collect();

    match parse(tokens) {
        Ok(tree) => {
            println!("{}", tree.to_dot());
        }
        Err(e) => {
            eprintln!("{e}");
        }
    }

    Ok(())
}
