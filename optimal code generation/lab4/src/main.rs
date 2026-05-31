use anyhow::Result;
use std::env::args;
use std::fs;

use compiler::frontend::lexer::tokenize;
use compiler::frontend::parser::parse;
use compiler::out::cfg_to_dot;

fn main() -> Result<()> {
    let filename = args().nth(1).expect("expected filename");
    let data = fs::read_to_string(&filename).expect("failed to read");
    let tokens = tokenize(&data)?;
    let program = parse(tokens)?;
    let ssa = program.compile();
    println!("{}", cfg_to_dot(&ssa.cfg));
    Ok(())
}
