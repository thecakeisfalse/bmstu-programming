use crate::frontend::ast::{self, BinOp as AstBinOp};
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Rc<str>,
    pub n: usize,
    pub is_temp: bool,
}

impl Variable {
    fn new(name: &str) -> Self {
        Variable {
            name: Rc::from(name),
            n: 0,
            is_temp: false,
        }
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_temp {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}@{}", self.name, self.n)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Var(Variable),
    Const(i64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Var(v) => write!(f, "{v}"),
            Value::Const(n) => write!(f, "{n}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    BinOp {
        dst: Variable,
        lhs: Value,
        rhs: Value,
        op: AstBinOp,
    },
    Copy {
        dst: Variable,
        src: Value,
    },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::BinOp { dst, lhs, rhs, op } => write!(f, "{dst} = {lhs} {op} {rhs}"),
            Instruction::Copy { dst, src } => write!(f, "{dst} = {src}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PhiArg {
    pub pred_block: usize,
    pub var: Variable,
}

#[derive(Debug, Clone)]
pub struct Phi {
    pub dst: Variable,
    pub args: Vec<PhiArg>,
}

impl fmt::Display for Phi {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = phi(", self.dst)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "BB{}: {}", arg.pred_block, arg.var)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Jump(usize),
    Branch {
        cond: Value,
        true_id: usize,
        false_id: usize,
    },
    Return(Value),
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terminator::Jump(id) => write!(f, "jump BB{id}"),
            Terminator::Branch {
                cond,
                true_id,
                false_id,
            } => write!(f, "branch {cond} ? BB{true_id} : BB{false_id}"),
            Terminator::Return(val) => write!(f, "return {val}"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct BasicBlock {
    pub id: usize,
    pub phis: Vec<Phi>,
    pub instructions: Vec<Instruction>,
    pub term: Option<Terminator>,
    pub preds: Vec<usize>,
    pub succs: Vec<usize>,
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "BB{}:  [preds: {}]",
            self.id,
            self.preds
                .iter()
                .map(|p| format!("BB{p}"))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        for phi in &self.phis {
            writeln!(f, "  {phi}")?;
        }
        for inst in &self.instructions {
            writeln!(f, "  {inst}")?;
        }
        if let Some(term) = &self.term {
            writeln!(f, "  {term}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct CFG {
    pub blocks: Vec<BasicBlock>,
    pub entry_block: usize,
}

impl fmt::Display for CFG {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "entry: BB{}", self.entry_block)?;
        writeln!(f)?;
        for block in &self.blocks {
            writeln!(f, "{block}")?;
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct CFGBuilder {
    cfg: CFG,
    current_block: usize,
    temp_counter: usize,
}

impl CFGBuilder {
    pub fn build(program: &ast::Program) -> CFG {
        let builder = Self::default();
        builder.inner_build(program)
    }

    fn new_block(&mut self) -> usize {
        let id = self.cfg.blocks.len();
        self.cfg.blocks.push(BasicBlock {
            id,
            ..Default::default()
        });
        id
    }

    fn new_temp(&mut self) -> Variable {
        let n = self.temp_counter;
        self.temp_counter += 1;
        Variable {
            name: format!("t{n}").into(),
            n: 0,
            is_temp: true,
        }
    }

    fn add_instruction(&mut self, inst: Instruction) {
        self.cfg.blocks[self.current_block].instructions.push(inst);
    }

    fn add_edge(&mut self, from: usize, to: usize) {
        self.cfg.blocks[from].succs.push(to);
        self.cfg.blocks[to].preds.push(from);
    }

    fn set_term(&mut self, term: Terminator) {
        let from = self.current_block;
        match &term {
            Terminator::Jump(to) => self.add_edge(from, *to),
            Terminator::Branch {
                true_id, false_id, ..
            } => {
                self.add_edge(from, *true_id);
                self.add_edge(from, *false_id);
            }
            Terminator::Return(_) => {}
        }
        self.cfg.blocks[from].term = Some(term);
    }

    fn seal_with_jump(&mut self, target: usize) {
        if self.cfg.blocks[self.current_block].term.is_none() {
            self.set_term(Terminator::Jump(target));
        }
    }

    fn inner_build(mut self, program: &ast::Program) -> CFG {
        let entry = self.new_block();
        self.cfg.entry_block = entry;
        self.current_block = entry;
        self.lower_block(&program.body);
        self.cfg
    }

    fn lower_block(&mut self, block: &ast::Block) {
        for stmt in &block.stmts {
            self.lower_stmt(stmt);
        }
    }

    fn lower_var_decl(&mut self, name: &str, init: &ast::Expr) {
        let val = self.lower_expr(init);
        self.add_instruction(Instruction::Copy {
            dst: Variable::new(name),
            src: val,
        });
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Expr(expr) => {
                self.lower_expr(expr);
            }
            ast::Stmt::VarDecl { name, init } => {
                self.lower_var_decl(name, init);
            }
            ast::Stmt::Return(expr) => {
                let val = self.lower_expr(expr);
                self.set_term(Terminator::Return(val));
            }
            ast::Stmt::Block(block) => {
                self.lower_block(block);
            }
            ast::Stmt::If { cond, then, else_ } => {
                let cond = self.lower_expr(cond);
                let then_id = self.new_block();
                let else_id = else_.as_ref().map(|_| self.new_block());
                let merge_id = self.new_block();

                let false_id = else_id.unwrap_or(merge_id);
                self.set_term(Terminator::Branch {
                    cond,
                    true_id: then_id,
                    false_id,
                });

                // then block
                self.current_block = then_id;
                self.lower_block(then);
                self.seal_with_jump(merge_id);

                // else block
                if let (Some(block), Some(else_id)) = (else_, else_id) {
                    self.current_block = else_id;
                    self.lower_block(block);
                    self.seal_with_jump(merge_id);
                }

                self.current_block = merge_id;
            }
            ast::Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl { name, init } => self.lower_var_decl(name, init),
                        ast::ForInit::Expr(expr) => {
                            self.lower_expr(expr);
                        }
                    }
                }

                let cond_id = self.new_block();
                let body_id = self.new_block();
                let exit_id = self.new_block();

                self.set_term(Terminator::Jump(cond_id));

                // cond
                self.current_block = cond_id;
                let cond_val = self.lower_expr(cond);
                self.set_term(Terminator::Branch {
                    cond: cond_val,
                    true_id: body_id,
                    false_id: exit_id,
                });

                // body
                self.current_block = body_id;
                self.lower_block(body);
                self.lower_expr(step);
                self.seal_with_jump(cond_id);

                // exit
                self.current_block = exit_id;
            }
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> Value {
        match expr {
            ast::Expr::IntLiteral(n) => Value::Const(*n),
            ast::Expr::Var(name) => Value::Var(Variable::new(name)),
            ast::Expr::Assign { name, value } => {
                let val = self.lower_expr(value);
                let dst = Variable::new(name);
                self.add_instruction(Instruction::Copy {
                    dst: dst.clone(),
                    src: val,
                });
                Value::Var(dst)
            }
            ast::Expr::UnaryNeg(op) => {
                let val = self.lower_expr(op);
                let dst = self.new_temp();
                self.add_instruction(Instruction::BinOp {
                    dst: dst.clone(),
                    lhs: Value::Const(0),
                    rhs: val,
                    op: AstBinOp::Sub,
                });
                Value::Var(dst)
            }
            ast::Expr::BinOp { left, right, op } => {
                let lhs = self.lower_expr(left);
                let rhs = self.lower_expr(right);
                let dst = self.new_temp();

                self.add_instruction(Instruction::BinOp {
                    dst: dst.clone(),
                    lhs,
                    rhs,
                    op: *op,
                });

                Value::Var(dst)
            }
        }
    }
}
