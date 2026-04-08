import abc
import re
import sys

from dataclasses import dataclass
from pprint import pprint

import parser_edsl as pe


# Type = Name TypeArgs
@dataclass
class Type:
    name: str
    arg_types: list[str]


# TypeDef = "type" Name ":" Types "."
@dataclass
class TypeDef:
    name: str
    variants: list[Type]


# Pattern = Name | "[" Name Patterns "]"
class Pattern(abc.ABC):
    pass


@dataclass
class VarPattern(Pattern):
    name: str


@dataclass
class ComplPattern(Pattern):
    name: str
    args: list[Pattern]


# Expr = Integer | Name | "(" Name Exprs ")" | "[" Name Exprs "]"
class Expr(abc.ABC):
    pass


@dataclass
class VarExpr(Expr):
    name: str


@dataclass
class IntExpr(Expr):
    value: int


@dataclass
class CallExpr(Expr):
    name: str
    args: list[Expr]


@dataclass
class ComplExpr(Expr):
    name: str
    args: list[Expr]


@dataclass
class MatchClause:
    pattern: Pattern
    body: Expr


@dataclass
class MatchBlock(Expr):
    var: str
    clauses: list[MatchClause]


# FunClause = "(" Name Patterns ")" "->" Expr
@dataclass
class FunClause:
    patterns: list[Pattern]
    body: Expr


# FunDef = "fun" "(" Name ArgNames ")" "->" Name ":" FunBody
@dataclass
class FunDef:
    name: str
    arg_types: list[str]
    return_type: str
    clauses: list[FunClause]


# Program = Program Definition | Definition
# Definition = TypeDef | FunDef
@dataclass
class Program:
    definitions: list[TypeDef | FunDef]


INTEGER = pe.Terminal("INTEGER", "[0-9]+", int, priority=7)
NAME = pe.Terminal("NAME", "[a-zA-Z_][a-zA-Z_0-9]*", str.upper)


def make_keyword(image) -> pe.Terminal:
    return pe.Terminal(
        image, image, lambda _: None, re_flags=re.IGNORECASE, priority=10
    )


KW_TYPE, KW_FUN, KW_MATCH = map(make_keyword, "type fun match".split())

NProgram, NDefinition = map(pe.NonTerminal, "Program Definition".split())

NTypeDef, NTypes, NType, NTypeArgs = map(
    pe.NonTerminal, "TypeDef Types Type TypeArgs".split()
)

NFunDef, NArgTypes, NFunBody, NFunClause = map(
    pe.NonTerminal, "FunDef ArgTypes FunBody FunClause".split()
)

NPatterns, NPattern = map(pe.NonTerminal, "Patterns Pattern".split())

NExprs, NExpr = map(pe.NonTerminal, "Exprs Expr".split())

NMatchClauses, NMatchClause = map(pe.NonTerminal, "MatchClauses MathClause".split())

# Program = Definition | Program Definition
NProgram |= NDefinition, lambda d: Program([d])
NProgram |= NProgram, NDefinition, lambda p, d: Program(p.definitions + [d])

# Definition = TypeDef | FunDef
NDefinition |= NTypeDef
NDefinition |= NFunDef

# TypeDef = "type" Name ":" Types "."
NTypeDef |= KW_TYPE, NAME, ":", NTypes, ".", TypeDef

# Types = Type | Types "|" Type
NTypes |= NType, lambda v: [v]
NTypes |= NTypes, "|", NType, lambda vs, v: vs + [v]

# Type = Name TypeArgs
NType |= NAME, NTypeArgs, Type

# TypeArgs = epsilon | TypeArgs Name
NTypeArgs |= lambda: []
NTypeArgs |= NTypeArgs, NAME, lambda args, n: args + [n]

# FunDef = "fun" "(" Name ArgTypes ")" "->" Name ":" FunBody "."
NFunDef |= (KW_FUN, "(", NAME, NArgTypes, ")", "->", NAME, ":", NFunBody, ".", FunDef)

# ArgTypes = Name | ArgTypes Name
NArgTypes |= NAME, lambda n: [n]
NArgTypes |= NArgTypes, NAME, lambda args, n: args + [n]

# FunBody = FunClause | FunBody "|" FunClause
NFunBody |= NFunClause, lambda c: [c]
NFunBody |= NFunBody, "|", NFunClause, lambda cs, c: cs + [c]

# FunClause = "(" Name Patterns ")" "->" Expr
NFunClause |= (
    "(",
    NAME,
    NPatterns,
    ")",
    "->",
    NExpr,
    lambda _, patterns, expr: FunClause(patterns, expr),
)

# Patterns = epsilon | Patterns Pattern
NPatterns |= lambda: []
NPatterns |= NPatterns, NPattern, lambda ps, p: ps + [p]

# Pattern = Name | "[" Name Patterns "]"
NPattern |= NAME, VarPattern
NPattern |= "[", NAME, NPatterns, "]", ComplPattern

# Exprs = epsilon | Exprs Expr
NExprs |= lambda: []
NExprs |= NExprs, NExpr, lambda es, e: es + [e]

# Expr = Name | Integer | "(" Name Exprs ")" | "[" Name Exprs "]"
NExpr |= NAME, KW_MATCH, NMatchClauses, ".", MatchBlock
NExpr |= NAME, VarExpr
NExpr |= INTEGER, IntExpr
NExpr |= "(", NAME, NExprs, ")", CallExpr
NExpr |= "[", NAME, NExprs, "]", ComplExpr

NMatchClauses |= NMatchClause, lambda c: [c]
NMatchClauses |= NMatchClauses, "|", NMatchClause, lambda cs, c: cs + [c]

NMatchClause |= NPattern, "->", NExpr, MatchClause

p = pe.Parser(NProgram, method=pe.EARLEY)

p.add_skipped_domain("\\s")
p.add_skipped_domain(r"<<.*>>")

filename = sys.argv[1]
with open(filename) as f:
    tree = p.parse(f.read())
    pprint(tree)
