use crate::lexer::Token;
use std::fmt::{self, Debug};

#[derive(Debug, Clone)]
pub enum Sym<T> {
    T(T),
    N(String),
}

#[derive(Debug)]
pub enum ParseTree<T> {
    Inner(String, Vec<ParseTree<T>>),
    Leaf(Option<Token<T>>),
}

pub struct ParseErr {
    msg: String,
    pos: Option<(usize, usize)>,
}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pos {
            Some((l, c)) => write!(f, "error ({l},{c}): {}", self.msg),
            None => write!(f, "error: {}", self.msg),
        }
    }
}

fn top_down_parse<T, F>(
    tokens: &[Token<T>],
    start: &str,
    eof: T,
    dispatch: F,
) -> Result<Vec<(String, Vec<Sym<T>>)>, ParseErr>
where
    T: Copy + PartialEq + Debug,
    F: Fn(&str, T) -> Option<Vec<Sym<T>>>,
{
    let cur = |i: usize| tokens.get(i).map(|t| t.kind).unwrap_or(eof);

    let mut rules: Vec<(String, Vec<Sym<T>>)> = vec![];
    let mut stack: Vec<Sym<T>> = vec![Sym::N(start.into())];
    let mut i = 0;

    loop {
        match stack.last().cloned() {
            None => break,
            Some(Sym::T(expected)) => {
                if cur(i) != expected {
                    return Err(ParseErr {
                        msg: format!("expected {expected:?}, got {:?}", cur(i)),
                        pos: tokens.get(i).map(|t| t.pos),
                    });
                }
                stack.pop();
                i += 1;
            }
            Some(Sym::N(nt)) => match dispatch(&nt, cur(i)) {
                None => {
                    return Err(ParseErr {
                        msg: format!("unexpected {:?} for nonterminal '{nt}'", cur(i)),
                        pos: tokens.get(i).map(|t| t.pos),
                    });
                }
                Some(rhs) => {
                    stack.pop();
                    for s in rhs.iter().rev() {
                        stack.push(s.clone());
                    }
                    rules.push((nt, rhs));
                }
            },
        }
    }

    if i < tokens.len() {
        return Err(ParseErr {
            msg: format!("expected end of file, found {:?}", tokens[i].kind),
            pos: Some(tokens[i].pos),
        });
    }

    Ok(rules)
}

fn build_tree<T>(
    sym: Sym<T>,
    rules: &mut impl Iterator<Item = (String, Vec<Sym<T>>)>,
    tokens: &mut impl Iterator<Item = Token<T>>,
) -> ParseTree<T> {
    match sym {
        Sym::T(_) => ParseTree::Leaf(tokens.next()),
        Sym::N(nt) => {
            let (_, rhs) = rules.next().unwrap();
            if rhs.is_empty() {
                ParseTree::Inner(nt, vec![ParseTree::Leaf(None)])
            } else {
                let children = rhs
                    .into_iter()
                    .map(|s| build_tree(s, rules, tokens))
                    .collect();
                ParseTree::Inner(nt, children)
            }
        }
    }
}

pub fn parse<T, F>(
    tokens: Vec<Token<T>>,
    start: &str,
    eof: T,
    dispatch: F,
) -> Result<ParseTree<T>, ParseErr>
where
    T: Copy + PartialEq + Debug,
    F: Fn(&str, T) -> Option<Vec<Sym<T>>>,
{
    let rules = top_down_parse(&tokens, start, eof, dispatch)?;
    Ok(build_tree(
        Sym::N(start.into()),
        &mut rules.into_iter(),
        &mut tokens.into_iter(),
    ))
}
