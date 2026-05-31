use crate::ir::CFG;

pub struct Graph {
    pub preds: Vec<Vec<usize>>,
    pub succs: Vec<Vec<usize>>,
    pub entry: usize,
}

impl Graph {
    pub fn len(&self) -> usize {
        self.succs.len()
    }

    fn dfs(&self, v: usize, visited: &mut Vec<bool>, postorder: &mut Vec<usize>) {
        visited[v] = true;
        for &u in &self.succs[v] {
            if !visited[u] {
                self.dfs(u, visited, postorder);
            }
        }
        postorder.push(v);
    }

    pub fn rpo(&self) -> Vec<usize> {
        let mut visited = vec![false; self.len()];
        let mut postorder = vec![];
        self.dfs(self.entry, &mut visited, &mut postorder);
        postorder.reverse();
        postorder
    }
}

impl From<&CFG> for Graph {
    fn from(cfg: &CFG) -> Self {
        let succs: Vec<_> = cfg.blocks.iter().map(|b| b.succs.clone()).collect();
        let preds: Vec<_> = cfg.blocks.iter().map(|b| b.preds.clone()).collect();

        Graph {
            succs,
            preds,
            entry: cfg.entry_block,
        }
    }
}
