use std::collections::hash_map::Entry;
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::fmt;

fn compress_range(mut syms: Vec<char>) -> String {
    syms.sort();
    syms.dedup();

    let mut parts = vec![];
    let mut i = 0;

    while i < syms.len() {
        let start = syms[i];
        let mut end = start;

        while i + 1 < syms.len() && syms[i + 1] as u32 == syms[i] as u32 + 1 {
            i += 1;
            end = syms[i];
        }

        parts.push(if end as u32 - start as u32 > 1 {
            format!("{}-{}", start, end)
        } else if end != start {
            format!("{}|{}", start, end)
        } else {
            start.to_string()
        });
        i += 1;
    }

    format!("{:?}", parts.join("|"))
        .replace("\\", "\\\\")
        .replace("\\\\\"\"", "\\\"\"")
}

#[derive(Default, Debug)]
pub struct Nfa {
    symbols: HashSet<char>,
    transitions: HashMap<(usize, char), HashSet<usize>>,
    finals: HashMap<usize, usize>,
    initial: usize,
}

impl Nfa {
    pub fn add_transition(&mut self, from: usize, sym: char, to: usize) {
        self.symbols.insert(sym);
        self.transitions.entry((from, sym)).or_default().insert(to);
    }

    pub fn add_final(&mut self, state: usize, color: usize) {
        self.finals.insert(state, color);
    }

    fn determinate(&self) -> Dfa {
        let start = BTreeSet::from([self.initial]);
        let mut convert = HashMap::from([(start.clone(), 0)]);
        let mut queue = VecDeque::from([start.clone()]);
        let mut states = HashSet::from([0]);
        let mut finals = HashMap::new();
        let mut transitions = HashMap::new();

        let f = |set: &BTreeSet<usize>| -> Option<usize> {
            set.iter().filter_map(|s| self.finals.get(s)).min().copied()
        };

        if let Some(v) = f(&start) {
            finals.insert(self.initial, v);
        }

        while let Some(cur) = queue.pop_front() {
            let cur_id = convert[&cur];

            for &sym in &self.symbols {
                let mut next = BTreeSet::new();

                for &from in &cur {
                    if let Some(to) = self.transitions.get(&(from, sym)) {
                        next.extend(to);
                    }
                }

                if next.is_empty() {
                    continue;
                }

                let next_id = if let Some(&id) = convert.get(&next) {
                    id
                } else {
                    let id = convert.len();
                    states.insert(id);

                    if let Some(v) = f(&next) {
                        finals.insert(id, v);
                    }

                    convert.insert(next.clone(), id);
                    queue.push_back(next);

                    id
                };

                transitions.insert((cur_id, sym), next_id);
            }
        }

        Dfa {
            states,
            symbols: self.symbols.clone(),
            transitions,
            finals,
            initial: *convert.get(&start).unwrap(),
        }
    }
}

impl From<Nfa> for Dfa {
    fn from(nfa: Nfa) -> Self {
        nfa.determinate()
    }
}

impl fmt::Display for Nfa {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "digraph NFA {{")?;
        writeln!(f, "  rankdir=LR;")?;

        for (&s, &v) in &self.finals {
            writeln!(f, "  \"{s}\" [shape=doublecircle label=\"{s} ({v})\"];")?;
        }

        writeln!(f, "  node [shape = circle];")?;
        writeln!(f, "  start [shape = point];")?;
        writeln!(f, "  start -> \"{}\";", self.initial)?;

        let mut grouped: HashMap<(usize, usize), Vec<char>> = HashMap::new();
        for (&(from, sym), to) in &self.transitions {
            for &to in to {
                grouped.entry((from, to)).or_default().push(sym);
            }
        }

        for ((from, to), syms) in grouped {
            let label = compress_range(syms);
            writeln!(f, "  \"{from}\" -> \"{to}\" [label = {label:?}];")?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug)]
pub struct Dfa {
    states: HashSet<usize>,
    symbols: HashSet<char>,
    transitions: HashMap<(usize, char), usize>,
    finals: HashMap<usize, usize>,
    initial: usize,
}

impl Dfa {
    pub fn longest_match(&self, s: &str) -> Option<(usize, usize)> {
        let mut cur = self.initial;
        let mut last = self.finals.get(&cur).map(|&k| (k, 0));
        let mut index = 0;

        for ch in s.chars() {
            cur = match self.transitions.get(&(cur, ch)) {
                Some(&next) => next,
                None => break,
            };

            index += 1;
            if let Some(&color) = self.finals.get(&cur) {
                last = Some((color, index));
            }
        }

        last
    }

    fn add_trap(&mut self) {
        let trap = self.states.len();
        let mut changed = false;

        for &state in &self.states {
            for &sym in &self.symbols {
                if let Entry::Vacant(e) = self.transitions.entry((state, sym)) {
                    e.insert(trap);
                    changed = true;
                }
            }
        }

        if changed {
            for &sym in &self.symbols {
                self.transitions.insert((trap, sym), trap);
            }
            self.states.insert(trap);
        }
    }

    fn get_trap(&self) -> Option<usize> {
        let mut trap: Option<usize> = None;

        for &state in &self.states {
            if self.finals.contains_key(&state) {
                continue;
            }

            if self
                .symbols
                .iter()
                .all(|&sym| self.transitions.get(&(state, sym)) == Some(&state))
            {
                trap = Some(state);
            }
        }

        trap
    }

    fn minimize(self) -> Dfa {
        let mut inv: HashMap<(usize, char), HashSet<usize>> = HashMap::new();
        for (&(from, sym), &to) in &self.transitions {
            inv.entry((to, sym)).or_default().insert(from);
        }

        let mut partition: HashMap<Option<usize>, HashSet<usize>> = HashMap::new();
        for &state in &self.states {
            partition
                .entry(self.finals.get(&state).copied())
                .or_default()
                .insert(state);
        }
        let mut partition: Vec<_> = partition.into_values().collect();

        let mut class: HashMap<usize, usize> = partition
            .iter()
            .enumerate()
            .flat_map(|(i, b)| b.iter().map(move |&s| (s, i)))
            .collect();

        let mut queue: VecDeque<(usize, char)> = (0..partition.len())
            .flat_map(|i| self.symbols.iter().map(move |&c| (i, c)))
            .collect();

        let mut in_queue: HashSet<(usize, char)> = queue.iter().copied().collect();

        while let Some((block, sym)) = queue.pop_front() {
            in_queue.remove(&(block, sym));

            let mut involved: HashMap<usize, HashSet<usize>> = HashMap::new();

            for &q in &partition[block] {
                if let Some(set) = inv.get(&(q, sym)) {
                    for &r in set {
                        involved.entry(class[&r]).or_default().insert(r);
                    }
                }
            }

            for (i, inv_i) in involved {
                if inv_i.len() >= partition[i].len() {
                    continue;
                }

                let j = partition.len();
                for &r in &inv_i {
                    partition[i].remove(&r);
                }
                partition.push(inv_i);

                if partition[j].len() > partition[i].len() {
                    partition.swap(i, j);
                }

                for &r in &partition[j] {
                    *class.get_mut(&r).unwrap() = j;
                }

                for &sym in &self.symbols {
                    if in_queue.insert((j, sym)) {
                        queue.push_back((j, sym));
                    }
                }
            }
        }

        let mut dfa = Dfa {
            states: (0..partition.len()).collect(),
            symbols: self.symbols,
            finals: HashMap::new(),
            initial: class[&self.initial],
            transitions: HashMap::new(),
        };

        for (c, block) in partition.iter().enumerate() {
            let v = *block.iter().next().unwrap();
            if let Some(&u) = self.finals.get(&v) {
                dfa.finals.insert(c, u);
            }

            for &sym in &dfa.symbols {
                if let Some(&u) = self.transitions.get(&(v, sym)) {
                    dfa.transitions.insert((c, sym), class[&u]);
                }
            }
        }

        dfa
    }

    fn canonize(self) -> Dfa {
        let mut color: HashMap<usize, usize> = HashMap::new();
        let mut adj: HashMap<usize, HashSet<usize>> = HashMap::new();

        let trap = self.get_trap();

        for (&(from, _), &to) in self.transitions.iter() {
            if trap == Some(from) || trap == Some(to) {
                continue;
            }
            adj.entry(from).or_default().insert(to);
        }

        fn dfs(color: &mut HashMap<usize, usize>, adj: &HashMap<usize, HashSet<usize>>, u: usize) {
            if color.contains_key(&u) {
                return;
            }

            color.insert(u, color.len());

            if let Some(v) = adj.get(&u) {
                for &v in v {
                    dfs(color, adj, v);
                }
            }
        }

        dfs(&mut color, &adj, self.initial);

        if let Some(trap) = trap {
            color.insert(trap, color.len());
        }

        Dfa {
            states: self.states,
            symbols: self.symbols,
            initial: 0,
            finals: HashMap::from_iter(self.finals.into_iter().map(|(u, v)| (color[&u], v))),
            transitions: HashMap::from_iter(
                self.transitions
                    .into_iter()
                    .map(|((from, sym), to)| ((color[&from], sym), color[&to])),
            ),
        }
    }
}

impl fmt::Display for Dfa {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let trap = self.get_trap();

        writeln!(f, "digraph DFA {{")?;
        writeln!(f, "  rankdir=LR;")?;

        for (&s, &v) in &self.finals {
            writeln!(f, "  \"{s}\" [shape=doublecircle label=\"{s} ({v})\"];")?;
        }

        writeln!(f, "  node [shape = circle];")?;
        writeln!(f, "  start [shape = point];")?;
        writeln!(f, "  start -> \"{}\";", self.initial)?;

        let mut grouped: HashMap<(usize, usize), Vec<char>> = HashMap::new();
        for (&(from, sym), &to) in &self.transitions {
            grouped.entry((from, to)).or_default().push(sym);
        }

        for ((from, to), syms) in grouped {
            let label = compress_range(syms);
            if Some(from) != trap && Some(to) != trap {
                writeln!(f, "  \"{from}\" -> \"{to}\" [label = {label}];")?;
            }
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Pos(char, usize);

#[derive(Debug)]
enum RegexAst {
    Empty,
    Symbol(Pos),
    Alternative(Box<RegexAst>, Box<RegexAst>),
    Concat(Box<RegexAst>, Box<RegexAst>),
    Star(Box<RegexAst>),
}

impl RegexAst {
    fn contains_eps(&self) -> bool {
        match self {
            Self::Empty | Self::Star(_) => true,
            Self::Symbol(_) => false,
            Self::Alternative(l, r) => l.contains_eps() || r.contains_eps(),
            Self::Concat(l, r) => l.contains_eps() && r.contains_eps(),
        }
    }

    fn first(&self) -> HashSet<Pos> {
        match self {
            Self::Empty => HashSet::new(),
            Self::Symbol(p) => HashSet::from([*p]),
            Self::Alternative(l, r) => {
                let mut s = l.first();
                s.extend(r.first());
                s
            }
            Self::Star(i) => i.first(),
            Self::Concat(l, r) => {
                let mut s = l.first();
                if l.contains_eps() {
                    s.extend(r.first());
                }
                s
            }
        }
    }

    fn last(&self) -> HashSet<Pos> {
        match self {
            Self::Empty => HashSet::new(),
            Self::Symbol(p) => HashSet::from([*p]),
            Self::Alternative(l, r) => {
                let mut s = l.last();
                s.extend(r.last());
                s
            }
            Self::Star(i) => i.last(),
            Self::Concat(l, r) => {
                let mut s = r.last();
                if r.contains_eps() {
                    s.extend(l.last());
                }
                s
            }
        }
    }

    fn follow(&self) -> HashSet<(Pos, Pos)> {
        match self {
            Self::Empty | Self::Symbol(_) => HashSet::new(),
            Self::Alternative(l, r) => {
                let mut f = l.follow();
                f.extend(r.follow());
                f
            }
            Self::Star(i) => {
                let mut f = i.follow();
                for l in i.last() {
                    for &r in &i.first() {
                        f.insert((l, r));
                    }
                }
                f
            }
            Self::Concat(l, r) => {
                let mut f = l.follow();
                f.extend(r.follow());
                for la in l.last() {
                    for &rf in &r.first() {
                        f.insert((la, rf));
                    }
                }
                f
            }
        }
    }

    fn positions(&self) -> Vec<Pos> {
        match self {
            Self::Empty => vec![],
            Self::Symbol(p) => vec![*p],
            Self::Star(i) => i.positions(),
            Self::Concat(l, r) | Self::Alternative(l, r) => {
                let mut v = l.positions();
                v.extend(r.positions());
                v
            }
        }
    }

    fn linearize(&mut self, c: &mut usize) {
        match self {
            Self::Empty => {}
            Self::Symbol(p) => {
                p.1 = *c;
                *c += 1;
            }
            Self::Alternative(l, r) | Self::Concat(l, r) => {
                l.linearize(c);
                r.linearize(c);
            }
            Self::Star(i) => i.linearize(c),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum RegexToken {
    Symbol(char),
    Alternative,
    Concat,
    Star,
    LParen,
    RParen,
}

struct RegexTokens(Vec<RegexToken>);

impl RegexTokens {
    fn tokenize(s: &str) -> Result<Self, String> {
        let mut tokens = vec![];
        let mut chars = s.chars().peekable();

        while let Some(ch) = chars.next() {
            match ch {
                '\\' => {
                    let c = chars.next().ok_or("unexpected end after \\")?;
                    let c = match c {
                        'x' => {
                            let h1 = chars.next().ok_or("unexpected end in \\x")?;
                            let h2 = chars.next().ok_or("unexpected end in \\x")?;
                            let code = u8::from_str_radix(&format!("{h1}{h2}"), 16)
                                .map_err(|_| format!("invalid hex escape \\x{h1}{h2}"))?;
                            code as char
                        }
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        c => c,
                    };
                    tokens.push(RegexToken::Symbol(c));
                }
                '[' => {
                    tokens.push(RegexToken::LParen);
                    let mut lits: Vec<char> = Vec::new();

                    while chars.peek().is_some_and(|&c| c != ']') {
                        let c = chars.next().unwrap();

                        let c = if c == '\\' {
                            let c = chars.next().ok_or("unexpected end after \\")?;
                            match c {
                                'x' => {
                                    let h1 = chars.next().ok_or("unexpected end in \\x")?;
                                    let h2 = chars.next().ok_or("unexpected end in \\x")?;
                                    let code = u8::from_str_radix(&format!("{h1}{h2}"), 16)
                                        .map_err(|_| format!("invalid hex escape \\x{h1}{h2}"))?;
                                    code as char
                                }
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                c => c,
                            }
                        } else {
                            c
                        };

                        if chars.peek() == Some(&'-')
                            && chars.clone().nth(1).is_some_and(|c| c != ']')
                        {
                            chars.next();
                            let end = chars.next().unwrap();

                            let end = if end == '\\' {
                                let next = chars.next().ok_or("unexpected end after \\")?;
                                match next {
                                    'x' => {
                                        let h1 = chars.next().ok_or("unexpected end in \\x")?;
                                        let h2 = chars.next().ok_or("unexpected end in \\x")?;
                                        let code = u8::from_str_radix(&format!("{h1}{h2}"), 16)
                                            .map_err(|_| {
                                                format!("invalid hex escape \\x{h1}{h2}")
                                            })?;
                                        code as char
                                    }
                                    'n' => '\n',
                                    't' => '\t',
                                    'r' => '\r',
                                    c => c,
                                }
                            } else {
                                end
                            };

                            lits.extend(c..=end);
                        } else {
                            lits.push(c);
                        }
                    }
                    chars.next();

                    for (i, c) in lits.into_iter().enumerate() {
                        if i > 0 {
                            tokens.push(RegexToken::Alternative);
                        }
                        tokens.push(RegexToken::Symbol(c));
                    }

                    tokens.push(RegexToken::RParen);
                }
                '(' => tokens.push(RegexToken::LParen),
                ')' => tokens.push(RegexToken::RParen),
                '|' => tokens.push(RegexToken::Alternative),
                '*' => tokens.push(RegexToken::Star),
                c => tokens.push(RegexToken::Symbol(c)),
            };
        }

        Ok(Self(tokens))
    }

    fn add_concat(self) -> Self {
        let mut prev: Option<RegexToken> = None;
        let mut result = vec![];

        for token in self.0 {
            if let Some(prev) = prev {
                let l = matches!(
                    prev,
                    RegexToken::Symbol(_) | RegexToken::Star | RegexToken::RParen
                );
                let r = matches!(token, RegexToken::Symbol(_) | RegexToken::LParen);

                if l && r {
                    result.push(RegexToken::Concat);
                }
            }
            result.push(token);
            prev = Some(token);
        }

        Self(result)
    }

    fn shunting_yard(self) -> Self {
        let tokens = self.add_concat();

        let priority = |t: RegexToken| match t {
            RegexToken::Star => 3,
            RegexToken::Concat => 2,
            RegexToken::Alternative => 1,
            _ => 0,
        };

        let mut polish = vec![];
        let mut ops = vec![];

        for token in tokens.0 {
            match token {
                RegexToken::Symbol(_) => polish.push(token),
                RegexToken::LParen => ops.push(token),
                RegexToken::RParen => {
                    while let Some(o) = ops.pop() {
                        if o == RegexToken::LParen {
                            break;
                        }
                        polish.push(o);
                    }
                }
                op => {
                    let p = priority(op);

                    while ops
                        .last()
                        .is_some_and(|&t| t != RegexToken::LParen && priority(t) >= p)
                    {
                        polish.push(ops.pop().unwrap());
                    }

                    ops.push(op);
                }
            }
        }

        while let Some(op) = ops.pop() {
            polish.push(op);
        }

        Self(polish)
    }

    fn as_ast(&self) -> Result<RegexAst, String> {
        let mut stack: Vec<RegexAst> = vec![];

        for &token in &self.0 {
            match token {
                RegexToken::Symbol(c) => stack.push(RegexAst::Symbol(Pos(c, 0))),
                RegexToken::Concat => {
                    let (r, l) = (
                        stack.pop().unwrap_or(RegexAst::Empty),
                        stack.pop().unwrap_or(RegexAst::Empty),
                    );

                    stack.push(RegexAst::Concat(Box::new(l), Box::new(r)));
                }
                RegexToken::Alternative => {
                    let (r, l) = (
                        stack.pop().unwrap_or(RegexAst::Empty),
                        stack.pop().unwrap_or(RegexAst::Empty),
                    );

                    stack.push(RegexAst::Alternative(Box::new(l), Box::new(r)));
                }
                RegexToken::Star => {
                    let i = stack.pop().ok_or("expected expression before *")?;
                    stack.push(RegexAst::Star(Box::new(i)));
                }
                _ => {}
            }
        }

        Ok(stack.pop().unwrap_or(RegexAst::Empty))
    }
}

impl TryFrom<&str> for RegexAst {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let tokens = RegexTokens::tokenize(s)?;
        let polish = tokens.shunting_yard();
        polish.as_ast()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Position {
    row: usize,
    col: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.row, self.col)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Segment {
    start: Position,
    end: Position,
}

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    color: usize,
    text: &'a str,
    s: Segment,
}

#[derive(Debug)]
struct ScanError {
    p: Position,
    message: String,
}

impl fmt::Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error {}: {}", self.p, self.message)
    }
}

pub struct RegexScanner(Dfa);

impl RegexScanner {
    pub fn new(patterns: &[&str]) -> Self {
        let mut asts: Vec<RegexAst> = patterns.iter().map(|&p| p.try_into().unwrap()).collect();
        let mut counter = 1;

        for ast in &mut asts {
            ast.linearize(&mut counter);
        }

        let mut nfa = Nfa::default();

        for (index, ast) in asts.iter().enumerate() {
            nfa.symbols.extend(ast.positions().iter().map(|p| p.0));

            for &Pos(ch, i) in &ast.first() {
                nfa.add_transition(0, ch, i);
            }

            for &(Pos(_, u), Pos(ch, v)) in &ast.follow() {
                nfa.add_transition(u, ch, v);
            }

            for &Pos(_, i) in &ast.last() {
                nfa.finals.entry(i).or_insert(index);
            }

            if ast.contains_eps() {
                nfa.finals.entry(0).or_insert(index);
            }
        }

        let mut dfa: Dfa = nfa.into();
        dfa.add_trap();
        Self(dfa.minimize().canonize())
    }

    fn tokenize<'a>(
        &'a self,
        input: &'a str,
    ) -> impl Iterator<Item = Result<Token<'a>, ScanError>> {
        ScannerIter {
            scanner: self,
            input,
            index: 0,
            p: Position { col: 1, row: 1 },
        }
    }
}

impl fmt::Display for RegexScanner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

struct ScannerIter<'a> {
    scanner: &'a RegexScanner,
    input: &'a str,
    p: Position,
    index: usize,
}

impl<'a> Iterator for ScannerIter<'a> {
    type Item = Result<Token<'a>, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.input.len() {
            return None;
        }

        let start = self.p;
        match self.scanner.0.longest_match(&self.input[self.index..]) {
            Some((color, len)) => {
                let text = &self.input[self.index..self.index + len];

                for ch in text.chars() {
                    if ch == '\n' {
                        self.p.col = 1;
                        self.p.row += 1;
                    } else {
                        self.p.col += 1;
                    }

                    self.index += 1;
                }

                let end = Position {
                    col: self.p.col - 1,
                    row: self.p.row,
                };

                Some(Ok(Token {
                    color,
                    text,
                    s: Segment { start, end },
                }))
            }
            None => {
                let ch = self.input[self.index..].chars().next().unwrap();

                let err = Err(ScanError {
                    message: format!("unexpected character {:?}", ch),
                    p: self.p,
                });

                if ch == '\n' {
                    self.p.row += 1;
                    self.p.col = 1;
                } else {
                    self.p.col += 1;
                }

                self.index += 1;

                Some(err)
            }
        }
    }
}

fn main() {
    let filename = std::env::args().nth(1).expect("expected filename");
    let input = std::fs::read_to_string(&filename).expect("cannot read file");

    let scanner = RegexScanner::new(&[
        "[ \t\r\n][ \t\r\n]*",
        "(&&|\\|\\|)",
        "(for)|(forward)",
        "[a-zA-Z][a-zA-Z0-9]*",
        "[0-9][0-9]*",
        "\"((\\\\[\\x00-\\x7f])|[\\x00-\\x21\\x23-\\x5b\\x5d-\\x7f])*\"",
    ]);

    let domain = ["WS", "Ops", "Keywords", "Ident", "Number", "String"];

    for token in scanner.tokenize(&input) {
        match token {
            Ok(token) if token.color == 0 => {}
            Ok(token) => println!("{} {}: {}", domain[token.color], token.s, token.text),
            Err(err) => println!("{err}"),
        }
    }

    println!("{scanner}");
}
