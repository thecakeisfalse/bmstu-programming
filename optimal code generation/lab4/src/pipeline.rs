use crate::{
    analysis::{DominanceFrontier, DominatorTree, Dominators, Graph},
    frontend::ast::Program,
    ir::{CFG, CFGBuilder, SSABuilder},
};

pub struct LoweredCFG {
    pub cfg: CFG,
}

pub struct AnalyzedCFG {
    pub cfg: CFG,
    df: DominanceFrontier,
    dom_tree: DominatorTree,
}

pub struct SsaCFG {
    pub cfg: CFG,
}

impl Program {
    pub fn lower(&self) -> LoweredCFG {
        LoweredCFG {
            cfg: CFGBuilder::build(self),
        }
    }

    pub fn compile(&self) -> SsaCFG {
        self.lower().analyze().into_ssa()
    }
}

impl LoweredCFG {
    pub fn analyze(self) -> AnalyzedCFG {
        let graph = Graph::from(&self.cfg);
        let doms = Dominators::build(&graph);
        let df = DominanceFrontier::build(&graph, &doms);
        let dom_tree = DominatorTree::from(&doms);

        AnalyzedCFG {
            cfg: self.cfg,
            df,
            dom_tree,
        }
    }
}

impl AnalyzedCFG {
    pub fn into_ssa(mut self) -> SsaCFG {
        SSABuilder::build(&mut self.cfg, &self.df, &self.dom_tree);
        SsaCFG { cfg: self.cfg }
    }
}
