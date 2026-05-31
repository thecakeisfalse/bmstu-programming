use crate::analysis::Graph;
use std::collections::HashSet;

pub struct Dominators {
    pub dom: Vec<HashSet<usize>>,
}

impl Dominators {
    pub fn build(g: &Graph) -> Self {
        let n = g.len();
        let rpo = g.rpo();

        let all: HashSet<_> = (0..n).collect();
        let mut dom = vec![all; n];
        dom[g.entry] = HashSet::from([g.entry]);

        let mut changed = true;
        while changed {
            changed = false;

            for &v in &rpo {
                if v == g.entry {
                    continue;
                }

                let preds = &g.preds[v];

                let mut new_dom = preds[1..]
                    .iter()
                    .fold(dom[preds[0]].clone(), |acc, &p| &acc & &dom[p]);

                new_dom.insert(v);

                if new_dom != dom[v] {
                    dom[v] = new_dom;
                    changed = true;
                }
            }
        }

        Dominators { dom }
    }

    pub fn is_dom(&self, x: usize, y: usize) -> bool {
        self.dom[y].contains(&x)
    }

    pub fn idom(&self, y: usize) -> Option<usize> {
        self.dom[y]
            .iter()
            .filter(|&&x| x != y)
            .find(|&&x| {
                self.dom[y]
                    .iter()
                    .filter(|&&c| c != y && c != x)
                    .all(|&c| self.is_dom(c, x))
            })
            .copied()
    }
}
