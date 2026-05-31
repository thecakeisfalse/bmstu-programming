use std::error::Error;
use std::iter::Peekable;

use crate::frontend::ast::*;
use crate::frontend::lexer::{Token, TokenKind};

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.message)
    }
}

impl Error for ParseError {}

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

type Result<T> = std::result::Result<T, ParseError>;

impl<T: Iterator<Item = Token>> Parser<T> {
    fn new(iter: T) -> Self {
        Self {
            tokens: iter.peekable(),
        }
    }

    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn advance(&mut self) -> Token {
        self.tokens.next().unwrap()
    }

    fn check(&mut self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn match_tok(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(ParseError {
                message: format!("unexpected token '{}'", self.peek().lexeme),
            })
        }
    }

    fn parse_program(&mut self) -> Result<Program> {
        self.expect(TokenKind::KwInt)?;
        let name = self.expect(TokenKind::Ident)?.lexeme;
        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::RParen)?;
        let body = self.parse_block()?;
        Ok(Program { name, body })
    }

    fn parse_block(&mut self) -> Result<Block> {
        self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while !self.check(TokenKind::RBrace) && !self.check(TokenKind::Eof) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Block { stmts })
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        match self.peek().kind {
            TokenKind::KwInt => self.parse_var_decl(),
            TokenKind::KwIf => self.parse_if(),
            TokenKind::KwFor => self.parse_for(),
            TokenKind::KwReturn => self.parse_return(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_var_decl_inner(&mut self) -> Result<(String, Expr)> {
        let name = self.expect(TokenKind::Ident)?.lexeme;
        let init = if self.match_tok(TokenKind::Assign) {
            self.parse_expr()?
        } else {
            Expr::IntLiteral(0)
        };
        Ok((name, init))
    }

    fn parse_var_decl(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::KwInt)?;
        let (name, init) = self.parse_var_decl_inner()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::VarDecl { name, init })
    }

    fn parse_if(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::KwIf)?;
        self.expect(TokenKind::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;
        let then = self.parse_block()?;
        let else_ = if self.match_tok(TokenKind::KwElse) {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Stmt::If { cond, then, else_ })
    }

    fn parse_for(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::KwFor)?;
        self.expect(TokenKind::LParen)?;

        let init = if self.check(TokenKind::Semicolon) {
            self.advance();
            None
        } else if self.check(TokenKind::KwInt) {
            self.advance();
            let (name, init) = self.parse_var_decl_inner()?;
            self.expect(TokenKind::Semicolon)?;
            Some(ForInit::VarDecl { name, init })
        } else {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semicolon)?;
            Some(ForInit::Expr(expr))
        };

        let cond = if self.check(TokenKind::Semicolon) {
            Expr::IntLiteral(1)
        } else {
            self.parse_expr()?
        };
        self.expect(TokenKind::Semicolon)?;

        let step = if self.check(TokenKind::RParen) {
            Expr::IntLiteral(0)
        } else {
            self.parse_expr()?
        };
        self.expect(TokenKind::RParen)?;

        let body = self.parse_block()?;
        Ok(Stmt::For {
            init,
            cond,
            step,
            body,
        })
    }

    fn parse_return(&mut self) -> Result<Stmt> {
        self.expect(TokenKind::KwReturn)?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Return(value))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> Result<Expr> {
        let left = self.parse_cmp()?;
        if self.match_tok(TokenKind::Assign) {
            if let Expr::Var(name) = left {
                let value = self.parse_assign()?;
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                });
            } else {
                return Err(ParseError {
                    message: "invalid assignment target".to_string(),
                });
            }
        }
        Ok(left)
    }

    fn parse_cmp(&mut self) -> Result<Expr> {
        let mut left = self.parse_add()?;
        loop {
            let op = match self.peek().kind {
                TokenKind::Eq => BinOp::Eq,
                TokenKind::NotEq => BinOp::NotEq,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::LtEq => BinOp::LtEq,
                TokenKind::GtEq => BinOp::GtEq,
                _ => break,
            };
            self.advance();
            let right = self.parse_add()?;
            left = Expr::BinOp {
                left: Box::new(left),
                right: Box::new(right),
                op,
            };
        }
        Ok(left)
    }

    fn parse_add(&mut self) -> Result<Expr> {
        let mut left = self.parse_mul()?;
        while matches!(self.peek().kind, TokenKind::Plus | TokenKind::Minus) {
            let op = if self.advance().kind == TokenKind::Plus {
                BinOp::Add
            } else {
                BinOp::Sub
            };
            let right = self.parse_mul()?;
            left = Expr::BinOp {
                left: Box::new(left),
                right: Box::new(right),
                op,
            };
        }
        Ok(left)
    }

    fn parse_mul(&mut self) -> Result<Expr> {
        let mut left = self.parse_unary()?;
        while matches!(
            self.peek().kind,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent
        ) {
            let op = match self.advance().kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                _ => BinOp::Mod,
            };
            let right = self.parse_unary()?;
            left = Expr::BinOp {
                left: Box::new(left),
                right: Box::new(right),
                op,
            };
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        if self.match_tok(TokenKind::Minus) {
            let operand = self.parse_unary()?;
            return Ok(Expr::UnaryNeg(Box::new(operand)));
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        match self.peek().kind {
            TokenKind::IntLiteral => {
                let lexeme = self.advance().lexeme;
                let value = lexeme.parse().unwrap();
                Ok(Expr::IntLiteral(value))
            }
            TokenKind::Ident => Ok(Expr::Var(self.advance().lexeme)),
            TokenKind::LParen => {
                self.advance();
                let e = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(e)
            }
            _ => Err(ParseError {
                message: format!("unexpected token '{}'", self.peek().lexeme),
            }),
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program> {
    let mut p = Parser::new(tokens.into_iter());
    let prog = p.parse_program()?;
    if !p.check(TokenKind::Eof) {
        return Err(ParseError {
            message: "expected end of file".to_string(),
        });
    }
    Ok(prog)
}
