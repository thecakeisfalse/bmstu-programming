use crate::analysis::{DominanceFrontier, DominatorTree};
use crate::ir::ir::{CFG, Instruction, Phi, PhiArg, Terminator, Value, Variable};

use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

type Counters = HashMap<Rc<str>, usize>;
type Stacks = HashMap<Rc<str>, Vec<usize>>;

pub struct SSABuilder<'a> {
    cfg: &'a mut CFG,
    df: &'a DominanceFrontier,
    dom_tree: &'a DominatorTree,
}

impl<'a> SSABuilder<'a> {
    pub fn build(cfg: &'a mut CFG, df: &'a DominanceFrontier, dom_tree: &'a DominatorTree) {
        let mut builder = Self { cfg, df, dom_tree };
        builder.insert_phis();

        let entry = builder.cfg.entry_block;
        let mut counters = Counters::new();
        let mut stacks = Stacks::new();
        builder.traverse(entry, &mut counters, &mut stacks);
    }

    fn insert_phis(&mut self) {
        let mut definitions: HashMap<Rc<str>, HashSet<usize>> = HashMap::new();

        for block in &self.cfg.blocks {
            for inst in &block.instructions {
                let dst = match inst {
                    Instruction::BinOp { dst, .. } | Instruction::Copy { dst, .. } => dst,
                };

                if dst.is_temp {
                    continue;
                }

                definitions
                    .entry(dst.name.clone())
                    .or_default()
                    .insert(block.id);
            }
        }

        for (name, def_blocks) in &definitions {
            let mut worklist: VecDeque<usize> = def_blocks.iter().copied().collect();
            let mut has_phi: HashSet<usize> = HashSet::new();

            while let Some(b) = worklist.pop_front() {
                for &d in &self.df.df[b] {
                    if !has_phi.insert(d) {
                        continue;
                    }

                    let n_preds = self.cfg.blocks[d].preds.len();
                    self.cfg.blocks[d].phis.push(Phi {
                        dst: versioned(name.clone(), 0),
                        args: vec![
                            PhiArg {
                                pred_block: 0,
                                var: versioned(name.clone(), 0),
                            };
                            n_preds
                        ],
                    });

                    if !def_blocks.contains(&d) {
                        worklist.push_back(d);
                    }
                }
            }
        }
    }

    fn traverse(&mut self, v: usize, counters: &mut Counters, stacks: &mut Stacks) {
        let mut pushed: Vec<Rc<str>> = Vec::new();

        {
            let block = &mut self.cfg.blocks[v];

            for phi in &mut block.phis {
                rename_def(&mut phi.dst, counters, stacks, &mut pushed);
            }

            for inst in &mut block.instructions {
                match inst {
                    Instruction::BinOp { dst, lhs, rhs, .. } => {
                        rename_use(lhs, stacks);
                        rename_use(rhs, stacks);
                        rename_def(dst, counters, stacks, &mut pushed);
                    }
                    Instruction::Copy { dst, src } => {
                        rename_use(src, stacks);
                        rename_def(dst, counters, stacks, &mut pushed);
                    }
                }
            }

            if let Some(term) = &mut block.term {
                match term {
                    Terminator::Branch { cond, .. } => rename_use(cond, stacks),
                    Terminator::Return(val) => rename_use(val, stacks),
                    Terminator::Jump(_) => {}
                }
            }
        }

        for si in 0..self.cfg.blocks[v].succs.len() {
            let u = self.cfg.blocks[v].succs[si];
            let j = self.cfg.blocks[u]
                .preds
                .iter()
                .position(|&p| p == v)
                .expect("v must be a predecessor of its successor");

            for phi_i in 0..self.cfg.blocks[u].phis.len() {
                let name = self.cfg.blocks[u].phis[phi_i].dst.name.clone();
                let version = current_version(&name, stacks);
                self.cfg.blocks[u].phis[phi_i].args[j] = PhiArg {
                    pred_block: v,
                    var: versioned(name, version),
                };
            }
        }

        for ci in 0..self.dom_tree.adj[v].len() {
            let child = self.dom_tree.adj[v][ci];
            self.traverse(child, counters, stacks);
        }

        for name in &pushed {
            if let Some(stack) = stacks.get_mut(name) {
                stack.pop();
            }
        }
    }
}

fn new_version(name: &Rc<str>, counters: &mut Counters, stacks: &mut Stacks) -> usize {
    let counter = counters.entry(name.clone()).or_insert(0);
    let version = *counter;
    *counter += 1;
    stacks.entry(name.clone()).or_default().push(version);
    version
}

fn current_version(name: &Rc<str>, stacks: &Stacks) -> usize {
    stacks
        .get(name)
        .and_then(|s| s.last().copied())
        .expect("unknown variable name")
}

fn rename_use(val: &mut Value, stacks: &Stacks) {
    if let Value::Var(var) = val {
        if !var.is_temp {
            var.n = current_version(&var.name, stacks);
        }
    }
}

fn rename_def(
    dst: &mut Variable,
    counters: &mut Counters,
    stacks: &mut Stacks,
    pushed: &mut Vec<Rc<str>>,
) {
    if dst.is_temp {
        return;
    }
    dst.n = new_version(&dst.name, counters, stacks);
    pushed.push(dst.name.clone());
}

fn versioned(name: Rc<str>, n: usize) -> Variable {
    Variable {
        name,
        n,
        is_temp: false,
    }
}
