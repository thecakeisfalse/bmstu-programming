use lab::lexer::{Lexer, Rule};
use lab::parser::{ParseTree, Sym, parse};
use std::collections::HashMap;
use std::io;

mod generated_table;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Tok {
    Num,
    Plus,
    Star,
    LPar,
    RPar,
    Eof,
}

fn dispatch(nt: &str, tok: Tok) -> Option<Vec<Sym<Tok>>> {
    use std::sync::OnceLock;

    fn tok_name(tok: Tok) -> &'static str {
        match tok {
            Tok::Num => "n",
            Tok::Plus => "plus sign",
            Tok::Star => "star",
            Tok::LPar => "left paren",
            Tok::RPar => "right paren",
            Tok::Eof => "$",
        }
    }

    fn sym_from_str(s: &str) -> Sym<Tok> {
        if s.starts_with(|c: char| c.is_uppercase()) {
            Sym::N(s.to_string())
        } else {
            let tok = match s {
                "n" => Tok::Num,
                "plus sign" => Tok::Plus,
                "star" => Tok::Star,
                "left paren" => Tok::LPar,
                "right paren" => Tok::RPar,
                "$" => Tok::Eof,
                s => panic!("unknown terminal in table: {s}"),
            };
            Sym::T(tok)
        }
    }

    static TABLE: OnceLock<HashMap<String, HashMap<String, Vec<String>>>> = OnceLock::new();
    let t = TABLE.get_or_init(generated_table::get_parsing_table);
    let rhs = t.get(nt)?.get(tok_name(tok))?;
    Some(rhs.iter().map(|s| sym_from_str(s)).collect())
}

fn eval(tree: &ParseTree<Tok>) -> f64 {
    let ParseTree::Inner(_, ch) = tree else {
        unreachable!()
    };

    match ch.as_slice() {
        [ParseTree::Leaf(Some(t))] if t.kind == Tok::Num => t.value.parse().unwrap(),
        [ParseTree::Leaf(Some(l)), e, ParseTree::Leaf(Some(r))]
            if l.kind == Tok::LPar && r.kind == Tok::RPar =>
        {
            eval(e)
        }
        [head, tail] => {
            let h = eval(head);
            eval_tail(tail, h)
        }
        _ => unreachable!(),
    }
}

fn eval_tail(tree: &ParseTree<Tok>, acc: f64) -> f64 {
    let ParseTree::Inner(_, ch) = tree else {
        unreachable!()
    };

    match &ch[0] {
        ParseTree::Leaf(None) => acc,
        ParseTree::Leaf(Some(op)) => {
            let rhs = eval(&ch[1]);
            let v = match op.kind {
                Tok::Plus => acc + rhs,
                Tok::Star => acc * rhs,
                _ => unreachable!(),
            };
            eval_tail(&ch[2], v)
        }
        _ => unreachable!(),
    }
}

fn main() {
    let mut expr = String::new();
    io::stdin().read_line(&mut expr).expect("read stdin");

    let lexer = Lexer::new(vec![
        Rule::new(r"^[0-9]+(\.[0-9]+)?", Tok::Num),
        Rule::new(r"^\+", Tok::Plus),
        Rule::new(r"^\*", Tok::Star),
        Rule::new(r"^\(", Tok::LPar),
        Rule::new(r"^\)", Tok::RPar),
    ]);

    let tokens: Vec<_> = lexer
        .iter(&expr)
        .filter_map(|r| r.map_err(|e| eprintln!("{e:?}")).ok())
        .collect();

    match parse(tokens, "E", Tok::Eof, dispatch) {
        Ok(tree) => println!("{}", eval(&tree)),
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}
