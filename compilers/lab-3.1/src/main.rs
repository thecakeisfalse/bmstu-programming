#[cfg(feature = "generated_table")]
mod generated_table;

use lab::lexer::{Lexer, Rule, Token};
use lab::parser::{Sym, parse};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::fs::OpenOptions;
use std::ops::{Deref, DerefMut};
use std::{env, fs, io::Write};

const EPS: &str = "epsilon";

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    KwTokens,
    KwIs,
    KwStart,
    LPar,
    RPar,
    Dot,
    Comma,
    Word,
    Comment,
    Eof,
}

fn transition(nt: &str, tok: TokenKind) -> Option<Vec<Sym<TokenKind>>> {
    use TokenKind::*;

    let rhs: &[&str] = match (nt, tok) {
        ("Program", KwTokens | LPar | KwStart | Eof) => &["StmtList"],
        ("StmtList", KwTokens | LPar | KwStart) => &["Stmt", "StmtList"],
        ("StmtList", Eof) => &[],
        ("Stmt", KwTokens) => &["TokensStmt"],
        ("Stmt", LPar) => &["RuleStmt"],
        ("Stmt", KwStart) => &["StartStmt"],
        ("TokensStmt", KwTokens) => &["$KwTokens", "TokenList", "$Dot"],
        ("TokenList", LPar) => &["ParenName", "TokenListTail"],
        ("TokenListTail", Comma) => &["$Comma", "ParenName", "TokenListTail"],
        ("TokenListTail", Dot) => &[],
        ("RuleStmt", LPar) => &["ParenName", "$KwIs", "Body", "Terminator"],
        ("Body", LPar) => &["ParenName", "Body"],
        ("Body", Dot | Comma) => &[],
        ("Terminator", Dot) => &["$Dot"],
        ("Terminator", Comma) => &["$Comma"],
        ("StartStmt", KwStart) => &["$KwStart", "ParenName", "$Dot"],
        ("ParenName", LPar) => &["$LPar", "NameWords", "$RPar"],
        ("NameWords", Word) => &["$Word", "NameWordsTail"],
        ("NameWordsTail", Word) => &["$Word", "NameWordsTail"],
        ("NameWordsTail", RPar) => &[],
        _ => return None,
    };

    Some(
        rhs.iter()
            .map(|&s| {
                if let Some(name) = s.strip_prefix('$') {
                    Sym::T(str_to_tok(name))
                } else {
                    Sym::N(s.into())
                }
            })
            .collect(),
    )
}

fn str_to_tok(s: &str) -> TokenKind {
    match s {
        "KwTokens" => TokenKind::KwTokens,
        "KwIs" => TokenKind::KwIs,
        "KwStart" => TokenKind::KwStart,
        "LPar" => TokenKind::LPar,
        "RPar" => TokenKind::RPar,
        "Dot" => TokenKind::Dot,
        "Comma" => TokenKind::Comma,
        "Word" => TokenKind::Word,
        s => panic!("unknown terminal: {s}"),
    }
}

#[cfg(not(feature = "generated_table"))]
fn dispatch(nt: &str, tok: TokenKind) -> Option<Vec<Sym<TokenKind>>> {
    transition(nt, tok)
}

#[cfg(feature = "generated_table")]
fn dispatch(nt: &str, tok: TokenKind) -> Option<Vec<Sym<TokenKind>>> {
    use std::sync::OnceLock;

    fn tok_name(tok: TokenKind) -> &'static str {
        match tok {
            TokenKind::KwTokens => "kw_tokens",
            TokenKind::KwIs => "kw_is",
            TokenKind::KwStart => "kw_start",
            TokenKind::LPar => "lpar",
            TokenKind::RPar => "rpar",
            TokenKind::Dot => "dot",
            TokenKind::Comma => "comma",
            TokenKind::Word => "word",
            TokenKind::Eof => "$",
            TokenKind::Comment => "comment",
        }
    }

    fn sym_from_str(s: &str) -> Sym<TokenKind> {
        if s.starts_with(|c: char| c.is_uppercase()) {
            Sym::N(s.to_string())
        } else {
            let tok = match s {
                "kw_tokens" => TokenKind::KwTokens,
                "kw_is" => TokenKind::KwIs,
                "kw_start" => TokenKind::KwStart,
                "lpar" => TokenKind::LPar,
                "rpar" => TokenKind::RPar,
                "dot" => TokenKind::Dot,
                "comma" => TokenKind::Comma,
                "word" => TokenKind::Word,
                "$" => TokenKind::Eof,
                s => panic!("unknown terminal in table: {s}"),
            };
            Sym::T(tok)
        }
    }

    static TABLE: OnceLock<HashMap<String, HashMap<String, Vec<String>>>> = OnceLock::new();
    let table = TABLE.get_or_init(generated_table::get_parsing_table);
    let rhs = table.get(nt)?.get(tok_name(tok))?;
    Some(rhs.iter().map(|s| sym_from_str(s)).collect())
}

#[derive(Debug, Default)]
struct Grammar {
    axiom: String,
    terminals: HashSet<String>,
    rules: Vec<(String, Vec<String>)>,
    nts: HashSet<String>,
}

type Sets = HashMap<String, HashSet<String>>;
type GResult<T> = Result<T, String>;

impl Grammar {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn extract(&mut self, toks: &[Token<TokenKind>]) -> GResult<()> {
        use TokenKind as O;

        let kind = |i: usize| toks.get(i).map(|t| t.kind);
        let mut i = 0;
        while i < toks.len() {
            match toks[i].kind {
                O::KwTokens => {
                    i += 1;
                    loop {
                        self.terminals.insert(Self::_read_name(&toks, &mut i));
                        i += 1;
                        if kind(i - 1) != Some(O::Comma) {
                            break;
                        }
                    }
                }
                O::KwStart => {
                    i += 1;
                    self.axiom = Self::_read_name(&toks, &mut i);
                    i += 1;
                }
                O::LPar => {
                    let lhs = Self::_read_name(&toks, &mut i);
                    i += 1;

                    let mut rhs = Vec::new();
                    while kind(i) == Some(O::LPar) {
                        rhs.push(Self::_read_name(&toks, &mut i));
                    }

                    self.rules.push((
                        lhs,
                        if rhs.is_empty() {
                            vec![EPS.into()]
                        } else {
                            rhs
                        },
                    ));
                }
                _ => i += 1,
            }
        }

        if self.axiom.is_empty() {
            return Err("axiom not found".to_string());
        }

        self.nts = self.rules.iter().map(|(l, _)| l.clone()).collect();

        Ok(self._validate()?)
    }

    fn _validate(&self) -> GResult<()> {
        for (lhs, rhs) in &self.rules {
            for sym in rhs {
                if sym == EPS {
                    continue;
                }

                if !self.nts.contains(sym.as_str()) && !self.terminals.contains(sym.as_str()) {
                    return Err(format!("undefined symbol '{sym}' in rule '{lhs}'"));
                }
            }
        }

        Ok(())
    }

    fn _read_name(toks: &[Token<TokenKind>], i: &mut usize) -> String {
        *i += 1; // LPar
        let mut words = Vec::new();
        while toks.get(*i).is_some_and(|t| t.kind == TokenKind::Word) {
            words.push(toks[*i].value.clone());
            *i += 1;
        }
        *i += 1; // RPar
        words.join(" ")
    }

    fn _first_of_seq(&self, seq: &[String], first: &Sets) -> HashSet<String> {
        let mut res = HashSet::new();

        for sym in seq {
            if sym == EPS {
                continue;
            }

            if self.nts.contains(sym.as_str()) {
                res.extend(first[sym].iter().filter(|s| *s != EPS).cloned());

                if !first[sym].contains(EPS) {
                    return res;
                }
            } else {
                res.insert(sym.clone());
                return res;
            }
        }

        res.insert(EPS.into());
        res
    }

    fn first(&self) -> Sets {
        let mut first: Sets = self
            .nts
            .iter()
            .map(|n| (n.clone(), HashSet::new()))
            .collect();

        loop {
            let mut changed = false;

            for (lhs, rhs) in &self.rules {
                for f in self._first_of_seq(rhs, &first) {
                    changed |= first.get_mut(lhs).unwrap().insert(f);
                }
            }

            if !changed {
                return first;
            }
        }
    }

    fn follow(&self, first: &Sets) -> Sets {
        let mut follow: Sets = self
            .nts
            .iter()
            .map(|n| (n.clone(), HashSet::new()))
            .collect();

        follow.get_mut(&self.axiom).unwrap().insert("$".into());

        loop {
            let mut changed = false;

            for (lhs, rhs) in &self.rules {
                for (i, sym) in rhs.iter().enumerate() {
                    if !self.nts.contains(sym) {
                        continue;
                    }

                    let bf = self._first_of_seq(&rhs[i + 1..], first);
                    let mut add: HashSet<String> =
                        bf.iter().filter(|s| *s != EPS).cloned().collect();

                    if bf.contains(EPS) {
                        add.extend(follow[lhs].iter().cloned());
                    }

                    let target = follow.get_mut(sym).unwrap();
                    for f in add {
                        changed |= target.insert(f);
                    }
                }
            }

            if !changed {
                return follow;
            }
        }
    }

    fn ll1(&self) -> GResult<LL1Table> {
        fn put(table: &mut LL1Table, lhs: &str, a: &str, rhs: &[String]) -> GResult<()> {
            let cell = table
                .entry(lhs.into())
                .or_default()
                .entry(a.into())
                .or_default();

            if cell.is_empty() || cell.as_slice() == rhs {
                *cell = rhs.to_vec();
                Ok(())
            } else {
                Err(format!("conflict: M[{lhs}, {a}]: {cell:?} != {rhs:?}"))
            }
        }

        let mut table = LL1Table::new();
        let first = self.first();
        let follow = self.follow(&first);

        for (lhs, rhs) in &self.rules {
            let fr = self._first_of_seq(rhs, &first);
            for a in &fr {
                if a == EPS {
                    continue;
                }

                put(&mut table, lhs, a, rhs)?;
            }
            if fr.contains(EPS) {
                for b in follow.get(lhs.as_str()).into_iter().flatten() {
                    put(&mut table, lhs, b, rhs)?;
                }
            }
        }

        Ok(table)
    }
}

#[derive(Default)]
struct LL1Table(HashMap<String, HashMap<String, Vec<String>>>);

impl LL1Table {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Deref for LL1Table {
    type Target = HashMap<String, HashMap<String, Vec<String>>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LL1Table {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for LL1Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "use std::collections::HashMap;\n")?;
        writeln!(
            f,
            "pub fn get_parsing_table() -> HashMap<String, HashMap<String, Vec<String>>> {{"
        )?;
        writeln!(
            f,
            "    let mut t: HashMap<String, HashMap<String, Vec<String>>> = HashMap::new();"
        )?;

        let mut lhs_vec: Vec<&String> = self.keys().collect();
        lhs_vec.sort();
        for lhs in lhs_vec {
            let row = &self[lhs];
            let mut terms: Vec<&String> = row.keys().collect();
            terms.sort();
            for term in terms {
                let rhs = &row[term];
                let is_eps = rhs == &[EPS.to_string()];
                let rhs_lit = if is_eps {
                    String::new()
                } else {
                    rhs.iter()
                        .map(|s| format!("{:?}.into()", s))
                        .collect::<Vec<_>>()
                        .join(", ")
                };
                writeln!(
                    f,
                    "    t.entry({:?}.into()).or_default().insert({:?}.into(), vec![{}]);",
                    lhs, term, rhs_lit
                )?;
            }
        }

        writeln!(f, "    t")?;
        writeln!(f, "}}")?;

        Ok(())
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} <file> [output file]", args[0]);
        std::process::exit(1);
    }

    let lexer = Lexer::new(vec![
        Rule::new(r"^\(\*[^*]*\*\)", TokenKind::Comment),
        Rule::new(r"^\(", TokenKind::LPar),
        Rule::new(r"^\)", TokenKind::RPar),
        Rule::new(r"^tokens\b", TokenKind::KwTokens),
        Rule::new(r"^is\b", TokenKind::KwIs),
        Rule::new(r"^start\b", TokenKind::KwStart),
        Rule::new(r"^\.", TokenKind::Dot),
        Rule::new(r"^,", TokenKind::Comma),
        Rule::new(r"^[a-zA-Z0-9_]+", TokenKind::Word),
    ]);

    let src = fs::read_to_string(&args[1]).unwrap_or_else(|e| {
        eprintln!("failed to read '{}': {e}", args[1]);
        std::process::exit(1);
    });

    let tokens: Vec<_> = lexer
        .iter(&src)
        .filter_map(|r| r.map_err(|e| eprintln!("{e:?}")).ok())
        .filter(|r| r.kind != TokenKind::Comment)
        .collect();

    if let Err(e) = parse(tokens.clone(), "Program", TokenKind::Eof, dispatch) {
        eprintln!("{e}");
        std::process::exit(1);
    };

    let mut g = Grammar::new();
    g.extract(&tokens).unwrap();

    let table = g.ll1().unwrap();

    let mut out = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open("src/generated_table.rs")?;

    writeln!(out, "{}", table)?;
    println!("success");

    Ok(())
}
