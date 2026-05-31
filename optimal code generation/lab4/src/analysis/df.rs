use std::collections::HashSet;

use crate::analysis::{Dominators, Graph};

#[derive(Debug)]
pub struct DominanceFrontier {
    pub df: Vec<HashSet<usize>>,
}

impl DominanceFrontier {
    pub fn build(g: &Graph, doms: &Dominators) -> Self {
        let n = g.len();

        let df = (0..n)
            .map(|x| {
                (0..n)
                    .filter(|&p| doms.is_dom(x, p))
                    .flat_map(|p| {
                        g.succs[p]
                            .iter()
                            .filter(|&&y| !(doms.is_dom(x, y) && x != y))
                            .copied()
                    })
                    .collect::<HashSet<_>>()
            })
            .collect::<Vec<_>>();

        DominanceFrontier { df }
    }
}
