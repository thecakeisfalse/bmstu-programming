use crate::ir::*;
use std::fmt::Write;

pub fn cfg_to_dot(cfg: &CFG) -> String {
    let mut out = String::new();
    writeln!(out, "digraph CFG {{").unwrap();
    writeln!(out, "    node [shape=box]").unwrap();

    for block in &cfg.blocks {
        writeln!(out, "    BB{} [label=\"{}\"]", block.id, block_label(block)).unwrap();
    }

    writeln!(out).unwrap();

    for block in &cfg.blocks {
        for &succ in &block.succs {
            writeln!(out, "    BB{} -> BB{}", block.id, succ).unwrap();
        }
    }

    writeln!(out, "}}").unwrap();
    out
}

fn block_label(block: &BasicBlock) -> String {
    let mut lines = vec![format!("BB{}", block.id), "---".to_string()];
    for phi in &block.phis {
        lines.push(format!("{phi}"));
    }
    for inst in &block.instructions {
        lines.push(format!("{inst}"));
    }
    if let Some(term) = &block.term {
        lines.push(format!("{term}"));
    }
    lines.join("\\l").replace('"', "\\\"") + "\\l"
}
