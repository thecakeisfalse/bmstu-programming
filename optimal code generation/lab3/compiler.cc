#include <fstream>
#include <memory>
#include <cctype>
#include <cstddef>
#include <expected>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include <print>
#include <ranges>
#include <unordered_map>
#include <variant>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

enum class TokenKind {
    IntLiteral,
    Ident,

    KwInt,
    KwIf,
    KwElse,
    KwFor,
    KwReturn,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,

    Assign,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,

    Eof
};

struct Position {
    std::size_t line;
    std::size_t col;
};

struct Token {
    TokenKind kind;
    std::string_view lexeme;
    Position p;
};

struct LexError {
    std::string message;
    Position p;
};

using LexResult = std::expected<Token, LexError>;

const std::unordered_map<std::string_view, TokenKind> keywords = {
    { "int",    TokenKind::KwInt    },
    { "if",     TokenKind::KwIf     },
    { "else",   TokenKind::KwElse   },
    { "for",    TokenKind::KwFor    },
    { "return", TokenKind::KwReturn },
};

const std::unordered_map<char, TokenKind> basic = {
    { '+', TokenKind::Plus      },
    { '-', TokenKind::Minus     },
    { '*', TokenKind::Star      },
    { '/', TokenKind::Slash     },
    { '%', TokenKind::Percent   },
    { '(', TokenKind::LParen    },
    { ')', TokenKind::RParen    },
    { '{', TokenKind::LBrace    },
    { '}', TokenKind::RBrace    },
    { ';', TokenKind::Semicolon },
};

struct LexerIterator {
    using iterator_category = std::input_iterator_tag;
    using value_type = LexResult;
    using difference_type = std::ptrdiff_t;

    LexerIterator() = default;

    LexerIterator(std::string_view src) : src_(src) { advance_token(); }

    const LexResult& operator*() const { return current_; }
    const LexResult* operator->() const { return &current_; }

    LexerIterator& operator++() {
        if (current_ && current_->kind == TokenKind::Eof) {
            done_ = true;
        } else {
            advance_token();
        }
        return *this;
    }

    LexerIterator operator++(int) {
        auto tmp = *this;
        ++*this;
        return tmp;
    }

    bool operator==(std::default_sentinel_t) const { return done_; }

  private:
    std::string_view src_;
    std::size_t pos_ = 0;
    std::size_t line_ = 1;
    std::size_t col_ = 1;
    LexResult current_;
    bool done_ = false;

    bool at_end() const { return pos_ >= src_.size(); }

    char peek(std::size_t offset = 0) const {
        return pos_ + offset < src_.size() ? src_[pos_ + offset] : '\0';
    }

    char advance() {
        char c = src_[pos_++];
        c == '\n' ? (++line_, col_ = 1) : ++col_;
        return c;
    }

    bool match(char expected) {
        if (peek() != expected) {
            return false;
        }
        advance();
        return true;
    }

    void skip_whitespace() {
        while (!at_end()) {
            if (std::isspace(peek())) {
                advance();
            } else if (peek() == '/' && peek(1) == '/') {
                while (!at_end() && peek() != '\n') {
                    advance();
                }
            } else {
                break;
            }
        }
    }

    void advance_token() {
        skip_whitespace();

        if (at_end()) {
            current_ = Token {
                TokenKind::Eof, "", { line_, col_ }
            };
            return;
        }

        std::size_t start = pos_;
        Position p { line_, col_ };
        char c = advance();

        auto make = [&](TokenKind kind) -> LexResult {
            return Token { kind, src_.substr(start, pos_ - start), p };
        };

        if (std::isdigit(c)) {
            while (std::isdigit(peek())) {
                advance();
            }
            current_ = make(TokenKind::IntLiteral);
            return;
        }

        if (std::isalpha(c) || c == '_') {
            while (std::isalnum(peek()) || peek() == '_') {
                advance();
            }
            auto word = src_.substr(start, pos_ - start);
            auto it = keywords.find(word);
            current_ = make(it != keywords.end() ? it->second : TokenKind::Ident);
            return;
        }

        if (auto it = basic.find(c); it != basic.end()) {
            current_ = make(it->second);
            return;
        }

        switch (c) {
            case '=':
                current_ = make(match('=') ? TokenKind::Eq : TokenKind::Assign);
                return;
            case '<':
                current_ = make(match('=') ? TokenKind::LtEq : TokenKind::Lt);
                return;
            case '>':
                current_ = make(match('=') ? TokenKind::GtEq : TokenKind::Gt);
                return;
            case '!':
                if (match('=')) {
                    current_ = make(TokenKind::NotEq);
                    return;
                }
            default:
                current_ = std::unexpected(
                    LexError { std::format("unexpected character '{}' at {}:{}", c, p.line, p.col),
                               p }
                );
        }
    }
};

auto lex(std::string_view src) {
    return std::ranges::subrange(LexerIterator(src), std::default_sentinel);
}

struct Expr;
struct Stmt;

struct IntLiteral {
    int value;
};

struct Var {
    std::string name;
};

struct BinOp {
    enum class Op { Add, Sub, Mul, Div, Mod, Eq, NotEq, Lt, Gt, LtEq, GtEq };

    std::unique_ptr<Expr> left;
    std::unique_ptr<Expr> right;
    Op op;
};

struct UnaryOp {
    enum class Op { Neg };

    std::unique_ptr<Expr> operand;
    Op op;
};

struct Assign {
    std::string name;
    std::unique_ptr<Expr> value;
};

using ExprKind = std::variant<IntLiteral, Var, BinOp, UnaryOp, Assign>;

struct Expr {
    ExprKind kind;
};

struct Block {
    std::vector<std::unique_ptr<Stmt>> stmts;
};

struct ExprStmt {
    std::unique_ptr<Expr> expr;
};

struct VarDecl {
    std::string name;
    std::optional<std::unique_ptr<Expr>> init;
};

struct IfStmt {
    std::unique_ptr<Expr> cond;
    Block then;
    std::optional<Block> else_;
};

using ForInit = std::variant<VarDecl, std::unique_ptr<Expr>>;

struct ForStmt {
    std::optional<ForInit> init;
    std::optional<std::unique_ptr<Expr>> cond;
    std::optional<std::unique_ptr<Expr>> step;
    Block body;
};

struct ReturnStmt {
    std::unique_ptr<Expr> value;
};

using StmtKind = std::variant<ExprStmt, VarDecl, Block, IfStmt, ForStmt, ReturnStmt>;

struct Stmt {
    StmtKind kind;
};

struct Program {
    std::string name;
    Block body;
};

struct ParseError {
    std::string message;
};

template <typename T>
using ParseResult = std::expected<T, ParseError>;

class Parser {
  public:
    Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)) {}

    ParseResult<Program> parse() {
        auto prog = parse_program();

        if (!prog) {
            return std::unexpected(prog.error());
        }

        if (!check(TokenKind::Eof)) {
            return err("expected end of file");
        }

        return std::move(*prog);
    }

  private:
    std::vector<Token> tokens_;
    std::size_t pos_ = 0;

    Token& peek() { return tokens_[pos_]; }
    Token advance() { return tokens_[pos_++]; }
    bool check(TokenKind kind) { return peek().kind == kind; }

    bool match(TokenKind kind) {
        if (!check(kind)) {
            return false;
        }
        advance();
        return true;
    }

    std::unexpected<ParseError> err(std::string msg) {
        return std::unexpected(ParseError { std::move(msg) });
    }

    ParseResult<Token> expect(TokenKind kind) {
        if (!check(kind)) {
            return err(std::format("wrong: {}", peek().lexeme));
        }
        return advance();
    }

    ParseResult<Program> parse_program() {
        if (auto r = expect(TokenKind::KwInt); !r) {
            return std::unexpected(r.error());
        }

        auto name = expect(TokenKind::Ident);

        if (!name) {
            return std::unexpected(name.error());
        }

        if (auto r = expect(TokenKind::LParen); !r) {
            return std::unexpected(r.error());
        }

        if (auto r = expect(TokenKind::RParen); !r) {
            return std::unexpected(r.error());
        }

        auto body = parse_block();

        if (!body) {
            return std::unexpected(body.error());
        }

        return Program { std::string(name->lexeme), std::move(*body) };
    }

    ParseResult<Block> parse_block() {
        if (auto r = expect(TokenKind::LBrace); !r) {
            return std::unexpected(r.error());
        }

        Block block;

        while (!check(TokenKind::RBrace) && !check(TokenKind::Eof)) {
            auto s = parse_stmt();

            if (!s) {
                return std::unexpected(s.error());
            }

            block.stmts.push_back(std::move(*s));
        }

        if (auto r = expect(TokenKind::RBrace); !r) {
            return std::unexpected(r.error());
        }

        return block;
    }

    ParseResult<std::unique_ptr<Stmt>> parse_stmt() {
        if (check(TokenKind::KwInt)) {
            return parse_var_decl();
        }

        if (check(TokenKind::KwIf)) {
            return parse_if();
        }

        if (check(TokenKind::KwFor)) {
            return parse_for();
        }

        if (check(TokenKind::KwReturn)) {
            return parse_return();
        }

        return parse_expr_stmt();
    }

    ParseResult<std::unique_ptr<Stmt>> parse_var_decl() {
        if (auto r = expect(TokenKind::KwInt); !r) {
            return std::unexpected(r.error());
        }

        auto name = expect(TokenKind::Ident);

        if (!name) {
            return std::unexpected(name.error());
        }

        std::optional<std::unique_ptr<Expr>> init;

        if (match(TokenKind::Assign)) {
            auto e = parse_expr();

            if (!e) {
                return std::unexpected(e.error());
            }

            init = std::move(*e);
        }

        if (auto r = expect(TokenKind::Semicolon); !r) {
            return std::unexpected(r.error());
        }

        return std::make_unique<Stmt>(Stmt {
            VarDecl { std::string(name->lexeme), std::move(init) }
        });
    }

    ParseResult<std::unique_ptr<Stmt>> parse_if() {
        if (auto r = expect(TokenKind::KwIf); !r) {
            return std::unexpected(r.error());
        }

        if (auto r = expect(TokenKind::LParen); !r) {
            return std::unexpected(r.error());
        }

        auto cond = parse_expr();

        if (!cond) {
            return std::unexpected(cond.error());
        }

        if (auto r = expect(TokenKind::RParen); !r) {
            return std::unexpected(r.error());
        }

        auto then = parse_block();

        if (!then) {
            return std::unexpected(then.error());
        }

        std::optional<Block> else_;

        if (match(TokenKind::KwElse)) {
            auto e = parse_block();

            if (!e) {
                return std::unexpected(e.error());
            }

            else_ = std::move(*e);
        }

        return std::make_unique<Stmt>(Stmt {
            IfStmt { std::move(*cond), std::move(*then), std::move(else_) }
        });
    }

    ParseResult<std::unique_ptr<Stmt>> parse_for() {
        if (auto r = expect(TokenKind::KwFor); !r) {
            return std::unexpected(r.error());
        }

        if (auto r = expect(TokenKind::LParen); !r) {
            return std::unexpected(r.error());
        }

        std::optional<ForInit> init;

        if (!check(TokenKind::Semicolon)) {
            if (check(TokenKind::KwInt)) {
                if (auto r = expect(TokenKind::KwInt); !r) {
                    return std::unexpected(r.error());
                }

                auto name = expect(TokenKind::Ident);

                if (!name) {
                    return std::unexpected(name.error());
                }

                std::optional<std::unique_ptr<Expr>> e;

                if (match(TokenKind::Assign)) {
                    auto expr = parse_expr();

                    if (!expr) {
                        return std::unexpected(expr.error());
                    }

                    e = std::move(*expr);
                }

                if (auto r = expect(TokenKind::Semicolon); !r) {
                    return std::unexpected(r.error());
                }

                init = VarDecl { std::string(name->lexeme), std::move(e) };
            } else {
                auto expr = parse_expr();

                if (!expr) {
                    return std::unexpected(expr.error());
                }

                if (auto r = expect(TokenKind::Semicolon); !r) {
                    return std::unexpected(r.error());
                }

                init = std::move(*expr);
            }
        } else {
            advance();
        }

        std::optional<std::unique_ptr<Expr>> cond;

        if (!check(TokenKind::Semicolon)) {
            auto e = parse_expr();

            if (!e) {
                return std::unexpected(e.error());
            }

            cond = std::move(*e);
        }

        if (auto r = expect(TokenKind::Semicolon); !r) {
            return std::unexpected(r.error());
        }

        std::optional<std::unique_ptr<Expr>> step;

        if (!check(TokenKind::RParen)) {
            auto e = parse_expr();

            if (!e) {
                return std::unexpected(e.error());
            }

            step = std::move(*e);
        }

        if (auto r = expect(TokenKind::RParen); !r) {
            return std::unexpected(r.error());
        }

        auto body = parse_block();

        if (!body) {
            return std::unexpected(body.error());
        }

        return std::make_unique<Stmt>(Stmt {
            ForStmt { std::move(init), std::move(cond), std::move(step), std::move(*body) }
        });
    }

    ParseResult<std::unique_ptr<Stmt>> parse_return() {
        if (auto r = expect(TokenKind::KwReturn); !r) {
            return std::unexpected(r.error());
        }

        auto value = parse_expr();

        if (!value) {
            return std::unexpected(value.error());
        }

        if (auto r = expect(TokenKind::Semicolon); !r) {
            return std::unexpected(r.error());
        }

        return std::make_unique<Stmt>(Stmt { ReturnStmt { std::move(*value) } });
    }

    ParseResult<std::unique_ptr<Stmt>> parse_expr_stmt() {
        auto e = parse_expr();

        if (!e) {
            return std::unexpected(e.error());
        }

        if (auto r = expect(TokenKind::Semicolon); !r) {
            return std::unexpected(r.error());
        }

        return std::make_unique<Stmt>(Stmt { ExprStmt { std::move(*e) } });
    }

    ParseResult<std::unique_ptr<Expr>> parse_expr() { return parse_assign(); }

    ParseResult<std::unique_ptr<Expr>> parse_assign() {
        if (check(TokenKind::Ident) && pos_ + 1 < tokens_.size() &&
            tokens_[pos_ + 1].kind == TokenKind::Assign) {
            auto name = std::string(advance().lexeme);
            advance();
            auto value = parse_assign();

            if (!value) {
                return std::unexpected(value.error());
            }

            return std::make_unique<Expr>(Expr {
                Assign { std::move(name), std::move(*value) }
            });
        }

        return parse_cmp();
    }

    ParseResult<std::unique_ptr<Expr>> parse_cmp() {
        auto left = parse_add();

        if (!left) {
            return std::unexpected(left.error());
        }

        while (true) {
            BinOp::Op op;

            switch (peek().kind) {
                case TokenKind::Eq:
                    op = BinOp::Op::Eq;
                    break;
                case TokenKind::NotEq:
                    op = BinOp::Op::NotEq;
                    break;
                case TokenKind::Lt:
                    op = BinOp::Op::Lt;
                    break;
                case TokenKind::Gt:
                    op = BinOp::Op::Gt;
                    break;
                case TokenKind::LtEq:
                    op = BinOp::Op::LtEq;
                    break;
                case TokenKind::GtEq:
                    op = BinOp::Op::GtEq;
                    break;
                default:
                    return left;
            }

            advance();

            auto right = parse_add();

            if (!right) {
                return std::unexpected(right.error());
            }

            left = std::make_unique<Expr>(Expr {
                BinOp { std::move(*left), std::move(*right), op }
            });
        }
    }

    ParseResult<std::unique_ptr<Expr>> parse_add() {
        auto left = parse_mul();

        if (!left) {
            return std::unexpected(left.error());
        }

        while (check(TokenKind::Plus) || check(TokenKind::Minus)) {
            auto op = advance().kind == TokenKind::Plus ? BinOp::Op::Add : BinOp::Op::Sub;
            auto right = parse_mul();

            if (!right) {
                return std::unexpected(right.error());
            }

            left = std::make_unique<Expr>(Expr {
                BinOp { std::move(*left), std::move(*right), op }
            });
        }

        return left;
    }

    ParseResult<std::unique_ptr<Expr>> parse_mul() {
        auto left = parse_unary();

        if (!left) {
            return std::unexpected(left.error());
        }

        while (check(TokenKind::Star) || check(TokenKind::Slash) || check(TokenKind::Percent)) {
            BinOp::Op op;

            switch (advance().kind) {
                case TokenKind::Star:
                    op = BinOp::Op::Mul;
                    break;
                case TokenKind::Slash:
                    op = BinOp::Op::Div;
                    break;
                default:
                    op = BinOp::Op::Mod;
                    break;
            }

            auto right = parse_unary();

            if (!right) {
                return std::unexpected(right.error());
            }

            left = std::make_unique<Expr>(Expr {
                BinOp { std::move(*left), std::move(*right), op }
            });
        }

        return left;
    }

    ParseResult<std::unique_ptr<Expr>> parse_unary() {
        if (match(TokenKind::Minus)) {
            auto operand = parse_unary();
            if (!operand) {
                return std::unexpected(operand.error());
            }
            return std::make_unique<Expr>(Expr {
                UnaryOp { std::move(*operand), UnaryOp::Op::Neg }
            });
        }
        return parse_primary();
    }

    ParseResult<std::unique_ptr<Expr>> parse_primary() {
        if (check(TokenKind::IntLiteral)) {
            auto tok = advance();
            int value = 0;

            for (char c : tok.lexeme) {
                value = value * 10 + (c - '0');
            }

            return std::make_unique<Expr>(Expr { IntLiteral { value } });
        }

        if (check(TokenKind::Ident)) {
            return std::make_unique<Expr>(Expr { Var { std::string(advance().lexeme) } });
        }

        if (match(TokenKind::LParen)) {
            auto e = parse_expr();

            if (!e) {
                return std::unexpected(e.error());
            }

            if (auto r = expect(TokenKind::RParen); !r) {
                return std::unexpected(r.error());
            }

            return e;
        }

        return err(std::format("unexpected token '{}'", peek().lexeme));
    }
};

std::expected<std::vector<Token>, LexError> tokenize(std::string_view src) {
    std::vector<Token> tokens;

    for (auto& r : lex(src)) {
        if (!r) {
            return std::unexpected(r.error());
        }

        tokens.push_back(*r);
    }

    return tokens;
}

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

class Codegen {
  public:
    Codegen()
        : context_(std::make_unique<llvm::LLVMContext>()),
          builder_(std::make_unique<llvm::IRBuilder<>>(*context_)),
          module_(std::make_unique<llvm::Module>("main", *context_)) {}

    const llvm::Module* get() const { return module_.get(); }

    void visit(const Program& program) {
        llvm::FunctionType* func_type =
            llvm::FunctionType::get(llvm::Type::getInt32Ty(*context_), {}, false);

        llvm::Function* main_func = llvm::Function::Create(
            func_type, llvm::Function::ExternalLinkage, program.name, module_.get()
        );

        llvm::BasicBlock* entry = llvm::BasicBlock::Create(*context_, "entrypoint", main_func);
        builder_->SetInsertPoint(entry);

        for (auto& s : program.body.stmts) {
            codegen_stmt(*s);
        }
    }

  private:
    std::unique_ptr<llvm::LLVMContext> context_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    std::unique_ptr<llvm::Module> module_;

    std::unordered_map<std::string, llvm::AllocaInst*> vars_;

    void codegen_var_decl(const VarDecl& v) {
        auto* alloca = builder_->CreateAlloca(llvm::Type::getInt32Ty(*context_), nullptr, v.name);
        vars_[v.name] = alloca;

        if (v.init) {
            auto* value = codegen_expr(**v.init);
            builder_->CreateStore(value, alloca);
        }
    }

    llvm::Value* codegen_expr(const Expr& expr) {
        return std::visit(
            overloaded {
                [&](const IntLiteral& n) -> llvm::Value* { return builder_->getInt32(n.value); },
                [&](const Var& v) -> llvm::Value* {
                    return builder_->CreateLoad(
                        llvm::Type::getInt32Ty(*context_), vars_.at(v.name)
                    );
                },
                [&](const BinOp& b) -> llvm::Value* {
                    auto* left = codegen_expr(*b.left);
                    auto* right = codegen_expr(*b.right);

                    switch (b.op) {
                        case BinOp::Op::Add:
                            return builder_->CreateAdd(left, right);
                        case BinOp::Op::Sub:
                            return builder_->CreateSub(left, right);
                        case BinOp::Op::Mul:
                            return builder_->CreateMul(left, right);
                        case BinOp::Op::Div:
                            return builder_->CreateSDiv(left, right);
                        case BinOp::Op::Mod:
                            return builder_->CreateSRem(left, right);
                        case BinOp::Op::Eq:
                            return builder_->CreateICmpEQ(left, right);
                        case BinOp::Op::NotEq:
                            return builder_->CreateICmpNE(left, right);
                        case BinOp::Op::Lt:
                            return builder_->CreateICmpSLT(left, right);
                        case BinOp::Op::Gt:
                            return builder_->CreateICmpSGT(left, right);
                        case BinOp::Op::LtEq:
                            return builder_->CreateICmpSLE(left, right);
                        case BinOp::Op::GtEq:
                            return builder_->CreateICmpSGE(left, right);
                    }

                    return nullptr;
                },
                [&](const UnaryOp& u) -> llvm::Value* {
                    auto* operand = codegen_expr(*u.operand);
                    return builder_->CreateNeg(operand);
                },
                [&](const Assign& a) -> llvm::Value* {
                    auto* value = codegen_expr(*a.value);
                    builder_->CreateStore(value, vars_.at(a.name));
                    return value;
                },
            },
            expr.kind
        );
    }

    llvm::Value* to_bool(llvm::Value* v) {
        if (v->getType()->isIntegerTy(1)) {
            return v;
        }

        return builder_->CreateICmpNE(v, builder_->getInt32(0));
    }

    void codegen_stmt(const Stmt& stmt) {
        std::visit(
            overloaded {
                [&](const ExprStmt& s) { codegen_expr(*s.expr); },
                [&](const VarDecl& s) { codegen_var_decl(s); },
                [&](const Block& s) {
                    for (auto& child : s.stmts) {
                        codegen_stmt(*child);
                    }
                },
                [&](const IfStmt& s) {
                    auto* cond_bool = to_bool(codegen_expr(*s.cond));

                    auto* fn = builder_->GetInsertBlock()->getParent();
                    auto* then_bb = llvm::BasicBlock::Create(*context_, "then", fn);
                    auto* else_bb = llvm::BasicBlock::Create(*context_, "else", fn);
                    auto* merge_bb = llvm::BasicBlock::Create(*context_, "merge", fn);

                    builder_->CreateCondBr(cond_bool, then_bb, else_bb);
                    builder_->SetInsertPoint(then_bb);

                    for (auto& child : s.then.stmts) {
                        codegen_stmt(*child);
                    }

                    builder_->CreateBr(merge_bb);
                    builder_->SetInsertPoint(else_bb);

                    if (s.else_) {
                        for (auto& child : s.else_->stmts) {
                            codegen_stmt(*child);
                        }
                    }

                    builder_->CreateBr(merge_bb);
                    builder_->SetInsertPoint(merge_bb);
                },
                [&](const ForStmt& s) {
                    auto* fn = builder_->GetInsertBlock()->getParent();
                    auto* cond_bb = llvm::BasicBlock::Create(*context_, "for.cond", fn);
                    auto* body_bb = llvm::BasicBlock::Create(*context_, "for.body", fn);
                    auto* end_bb = llvm::BasicBlock::Create(*context_, "for.end", fn);

                    if (s.init) {
                        std::visit(
                            overloaded {
                                [&](const VarDecl& v) { codegen_var_decl(v); },
                                [&](const std::unique_ptr<Expr>& e) { codegen_expr(*e); },
                            },
                            *s.init
                        );
                    }

                    builder_->CreateBr(cond_bb);
                    builder_->SetInsertPoint(cond_bb);

                    if (s.cond) {
                        auto* cond_bool = to_bool(codegen_expr(**s.cond));
                        builder_->CreateCondBr(cond_bool, body_bb, end_bb);
                    } else {
                        builder_->CreateBr(body_bb);
                    }

                    builder_->SetInsertPoint(body_bb);

                    for (auto& child : s.body.stmts) {
                        codegen_stmt(*child);
                    }

                    if (s.step) {
                        codegen_expr(**s.step);
                    }

                    builder_->CreateBr(cond_bb);
                    builder_->SetInsertPoint(end_bb);
                },
                [&](const ReturnStmt& s) {
                    auto* value = codegen_expr(*s.value);
                    builder_->CreateRet(value);
                },
            },
            stmt.kind
        );
    }
};

std::expected<std::string, std::string> read_file(const std::string& filename) {
    std::ifstream file(filename);

    if (!file.is_open()) {
        return std::unexpected("cannot open file");
    }

    std::string data;
    std::string line;

    while (std::getline(file, line)) {
        data += line;
    }

    return data;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::println("usage: {} <filename>", argv[0]);
        return 0;
    }

    auto data = read_file(argv[1]);

    if (!data) {
        std::println("read: {}", data.error());
        return 1;
    }

    auto tokens = tokenize(data.value());

    if (!tokens) {
        std::println("lexer: {}", tokens.error().message);
        return 1;
    }

    auto result = Parser(tokens.value()).parse();

    if (!result) {
        std::println("{}", result.error().message);
        return 1;
    }

    Codegen ir;
    ir.visit(result.value());

    auto module = ir.get();
    module->dump();

    return 0;
}
