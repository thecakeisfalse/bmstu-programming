use crate::analysis::Dominators;

#[derive(Debug)]
pub struct DominatorTree {
    pub idom: Vec<Option<usize>>,
    pub adj: Vec<Vec<usize>>,
}

impl From<&Dominators> for DominatorTree {
    fn from(doms: &Dominators) -> Self {
        let n = doms.dom.len();
        let idom: Vec<_> = (0..n).map(|v| doms.idom(v)).collect();
        let mut adj = vec![vec![]; n];

        for (v, &p) in idom.iter().enumerate() {
            if let Some(p) = p {
                adj[p].push(v);
            }
        }

        DominatorTree { idom, adj }
    }
}
