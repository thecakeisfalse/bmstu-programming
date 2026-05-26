import abc
import re
import sys

from dataclasses import dataclass, field

import parser_edsl as pe


class SemanticError(pe.Error):
    pass


class DuplicatePatternVar(SemanticError):
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    @property
    def message(self):
        return f"Переменная {self.name} встречается в образце несколько раз"


class UnknownTypeConstructor(SemanticError):
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    @property
    def message(self):
        return f"Неизвестный конструктор типа {self.name}"


class TypeConstructorArityMismatch(SemanticError):
    def __init__(self, pos, name, expected, got):
        self.pos = pos
        self.name = name
        self.expected = expected
        self.got = got

    @property
    def message(self):
        return f"Конструктор {self.name}: ожидалось {self.expected} аргументов, получено {self.got}"


class TypeMismatch(SemanticError):
    def __init__(self, pos, expected, got):
        self.pos = pos
        self.expected = expected
        self.got = got

    @property
    def message(self):
        return f"Ожидался тип {self.expected}, получен {self.got}"


class UnknownIdentifier(SemanticError):
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    @property
    def message(self):
        return f"Неизвестная переменная {self.name}"


class FunctionArityMismatch(SemanticError):
    def __init__(self, pos, name, expected, got):
        self.pos = pos
        self.name = name
        self.expected = expected
        self.got = got

    @property
    def message(self):
        return f"Функция {self.name}: ожидалось {self.expected} аргументов, получено {self.got}"


class ClauseNameMismatch(SemanticError):
    def __init__(self, pos, expected, got):
        self.pos = pos
        self.expected = expected
        self.got = got

    @property
    def message(self):
        return f"Неправильное имя в альтернативе функции: ожидалось {self.expected}, получено {self.got}"


class PatternArityMismatch(SemanticError):
    def __init__(self, pos, name, expected, got):
        self.pos = pos
        self.name = name
        self.expected = expected
        self.got = got

    @property
    def message(self):
        return f"Функция {self.name}: в сигнатуре {self.expected} аргументов, в образце {self.got}"


class DuplicateFunctionName(SemanticError):
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    @property
    def message(self):
        return f"Функция {self.name} определена повторно"


class DuplicateTypeName(SemanticError):
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    @property
    def message(self):
        return f"Тип {self.name} определен повторно"


class DuplicateTypeConstructor(SemanticError):
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    @property
    def message(self):
        return f"Конструктор {self.name} определен повторно"


class UnknownTypeName(SemanticError):
    def __init__(self, pos, name):
        self.pos = pos
        self.name = name

    @property
    def message(self):
        return f"Неизвестный тип {self.name}"


class SymbolTable:
    def __init__(self, parent=None):
        self._parent = parent
        self._table = {}

    def insert(self, ns, k, v):
        self._table[ns, k] = v

    def get(self, ns, k):
        if (ns, k) in self._table:
            return self._table[ns, k]

        if self._parent is not None:
            return self._parent.get(ns, k)

        return None

    def is_local(self, ns, k):
        return (ns, k) in self._table


# Type = Name TypeArgs
@dataclass
class Type:
    name: str
    arg_types: list[str]
    name_coord: pe.Position
    parent_type_name: str | None = field(default=None, init=False)

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        name, arg_types = attrs
        cname, _= coords
        return Type(name, arg_types, cname.start)


# TypeDef = "type" Name ":" Types "."
@dataclass
class TypeDef:
    name: str
    name_coord: pe.Position | None
    variants: list[Type]

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        name, variants = attrs
        _, cname, *_ = coords
        return TypeDef(name, cname.start, variants)

    def register(self, global_table: SymbolTable):
        if global_table.is_local("type", self.name):
            raise DuplicateTypeName(self.name_coord, self.name)

        global_table.insert("type", self.name, self)

        for v in self.variants:
            if global_table.is_local("value", v.name):
                raise DuplicateTypeConstructor(v.name_coord, v.name)

            v.parent_type_name = self.name
            global_table.insert("value", v.name, v)

    def check(self, table):
        for v in self.variants:
            for arg_type in v.arg_types:
                if not isinstance(table.get("type", arg_type), TypeDef):
                    raise UnknownTypeName(v.name_coord, arg_type)


# Pattern = Name | "[" Name Patterns "]"
class Pattern(abc.ABC):
    @abc.abstractmethod
    def check(self, expected, table: SymbolTable):
        pass


@dataclass
class VarPattern(Pattern):
    name: str
    coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        (name,) = attrs
        (cname,) = coords
        return VarPattern(name, cname.start)

    def check(self, expected, table):
        if table.is_local("value", self.name):
            raise DuplicatePatternVar(self.coord, self.name)

        self.type_name = expected
        table.insert("value", self.name, self)


@dataclass
class ComplPattern(Pattern):
    name: str
    name_coord: pe.Position
    args: list[Pattern]

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        name, args = attrs
        _, cname, *_ = coords
        return ComplPattern(name, cname.start, args)

    def check(self, expected, table):
        node = table.get("value", self.name)

        if not isinstance(node, Type):
            raise UnknownTypeConstructor(self.name_coord, self.name)

        if len(self.args) != len(node.arg_types):
            raise TypeConstructorArityMismatch(
                self.name_coord, self.name, len(node.arg_types), len(self.args)
            )

        if expected is not None and node.parent_type_name != expected:
            raise TypeMismatch(self.name_coord, expected, node.parent_type_name)

        for pattern, arg_type in zip(self.args, node.arg_types):
            pattern.check(arg_type, table)


# Expr = Integer | Name | "(" Name Exprs ")" | "[" Name Exprs "]"
class Expr(abc.ABC):
    type_name: str | None
    coord: pe.Position

    @abc.abstractmethod
    def check(self, table: SymbolTable):
        pass


@dataclass
class VarExpr(Expr):
    name: str
    coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        (name,) = attrs
        (cname,) = coords
        return VarExpr(name, cname.start)

    def check(self, table):
        node = table.get("value", self.name)

        if not isinstance(node, VarPattern):
            raise UnknownIdentifier(self.coord, self.name)

        self.type_name = node.type_name


@dataclass
class IntExpr(Expr):
    value: int
    coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        (value,) = attrs
        (cvalue,) = coords
        return IntExpr(value, cvalue.start)

    def check(self, table):
        self.type_name = "INT"


@dataclass
class CallExpr(Expr):
    name: str
    name_coord: pe.Position
    args: list[Expr]

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        name, args = attrs
        _, cname, *_ = coords
        return CallExpr(name, cname.start, args)

    def check(self, table):
        node = table.get("value", self.name)

        if not isinstance(node, FunDef):
            raise UnknownIdentifier(self.name_coord, self.name)

        if len(self.args) != len(node.arg_types):
            raise FunctionArityMismatch(
                self.name_coord, self.name, len(node.arg_types), len(self.args)
            )

        for expr, expected in zip(self.args, node.arg_types):
            expr.check(table)

            if expr.type_name != expected:
                raise TypeMismatch(expr.coord, expected, expr.type_name)

        self.type_name = node.return_type


@dataclass
class ComplExpr(Expr):
    name: str
    name_coord: pe.Position
    args: list[Expr]

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        name, args = attrs
        _, cname, *_ = coords
        return ComplExpr(name, cname.start, args)

    def check(self, table):
        node = table.get("value", self.name)

        if not isinstance(node, Type):
            raise UnknownIdentifier(self.name_coord, self.name)

        if len(self.args) != len(node.arg_types):
            raise TypeConstructorArityMismatch(
                self.name_coord, self.name, len(node.arg_types), len(self.args)
            )

        for expr, expected in zip(self.args, node.arg_types):
            expr.check(table)

            if expr.type_name != expected:
                raise TypeMismatch(expr.coord, expected, expr.type_name)

        self.type_name = node.parent_type_name


# FunClause = "(" Name Patterns ")" "->" Expr
@dataclass
class FunClause:
    fun_name: str
    fun_name_coord: pe.Position
    patterns: list[Pattern]
    body: Expr

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        fun_name, patterns, expr = attrs
        _, cname, *_ = coords
        return FunClause(fun_name, cname.start, patterns, expr)

    def check(self, expected_name, arg_types, return_type, table):
        if self.fun_name != expected_name:
            raise ClauseNameMismatch(self.fun_name_coord, expected_name, self.fun_name)

        if len(self.patterns) != len(arg_types):
            raise PatternArityMismatch(
                self.fun_name_coord, self.fun_name, len(arg_types), len(self.patterns)
            )

        table = SymbolTable(parent=table)
        for pattern, arg_type in zip(self.patterns, arg_types):
            pattern.check(arg_type, table)

        self.body.check(table)

        if self.body.type_name != return_type:
            raise TypeMismatch(self.fun_name_coord, return_type, self.body.type_name)


# FunDef = "fun" "(" Name ArgNames ")" "->" Name ":" FunBody
@dataclass
class FunDef:
    name: str
    name_coord: pe.Position | None
    arg_types: list[str]
    return_type: str
    clauses: list[FunClause]

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, _):
        fun_name, arg_types, return_type, clauses = attrs
        _, _, cname,  *_ = coords
        return FunDef(fun_name, cname.start, arg_types, return_type, clauses)

    def register(self, global_table: SymbolTable):
        if global_table.is_local("value", self.name):
            raise DuplicateFunctionName(self.name_coord, self.name)

        global_table.insert("value", self.name, self)

    def check(self, table):
        for clause in self.clauses:
            clause.check(self.name, self.arg_types, self.return_type, table)


BUILTIN_FUNCTIONS = {
    "ADD": (["INT", "INT"], "INT"),
    "MUL": (["INT", "INT"], "INT"),
    "SUB": (["INT", "INT"], "INT"),
    "DIV": (["INT", "INT"], "INT"),
}


# Program = Program Definition | Definition
# Definition = TypeDef | FunDef
@dataclass
class Program:
    definitions: list[TypeDef | FunDef]

    def check(self):
        global_table = SymbolTable()
        global_table.insert("type", "INT", TypeDef("INT", None, []))

        for name, (types, return_type) in BUILTIN_FUNCTIONS.items():
            global_table.insert(
                "value", name, FunDef(name, None, types, return_type, [])
            )

        for defn in self.definitions:
            defn.register(global_table)

        for defn in self.definitions:
            defn.check(global_table)


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

# Program = Definition | Program Definition
NProgram |= NDefinition, lambda d: Program([d])
NProgram |= NProgram, NDefinition, lambda p, d: Program(p.definitions + [d])

# Definition = TypeDef | FunDef
NDefinition |= NTypeDef
NDefinition |= NFunDef

# TypeDef = "type" Name ":" Types "."
NTypeDef |= KW_TYPE, NAME, ":", NTypes, ".", TypeDef.create

# Types = Type | Types "|" Type
NTypes |= NType, lambda v: [v]
NTypes |= NTypes, "|", NType, lambda vs, v: vs + [v]

# Type = Name TypeArgs
NType |= NAME, NTypeArgs, Type.create

# TypeArgs = epsilon | TypeArgs Name
NTypeArgs |= lambda: []
NTypeArgs |= NTypeArgs, NAME, lambda args, n: args + [n]

# FunDef = "fun" "(" Name ArgTypes ")" "->" Name ":" FunBody "."
NFunDef |= (
    KW_FUN,
    "(",
    NAME,
    NArgTypes,
    ")",
    "->",
    NAME,
    ":",
    NFunBody,
    ".",
    FunDef.create,
)

# ArgTypes = Name | ArgTypes Name
NArgTypes |= NAME, lambda n: [n]
NArgTypes |= NArgTypes, NAME, lambda args, n: args + [n]

# FunBody = FunClause | FunBody "|" FunClause
NFunBody |= NFunClause, lambda c: [c]
NFunBody |= NFunBody, "|", NFunClause, lambda cs, c: cs + [c]

# FunClause = "(" Name Patterns ")" "->" Expr
NFunClause |= "(", NAME, NPatterns, ")", "->", NExpr, FunClause.create

# Patterns = epsilon | Patterns Pattern
NPatterns |= lambda: []
NPatterns |= NPatterns, NPattern, lambda ps, p: ps + [p]

# Pattern = Name | "[" Name Patterns "]"
NPattern |= NAME, VarPattern.create
NPattern |= "[", NAME, NPatterns, "]", ComplPattern.create

# Exprs = epsilon | Exprs Expr
NExprs |= lambda: []
NExprs |= NExprs, NExpr, lambda es, e: es + [e]

# Expr = Name | Integer | "(" Name Exprs ")" | "[" Name Exprs "]"
NExpr |= NAME, VarExpr.create
NExpr |= INTEGER, IntExpr.create
NExpr |= "(", NAME, NExprs, ")", CallExpr.create
NExpr |= "[", NAME, NExprs, "]", ComplExpr.create

p = pe.Parser(NProgram, method=pe.EARLEY)

p.add_skipped_domain("\\s")
p.add_skipped_domain(r"<<.*>>")

filename = sys.argv[1]
with open(filename) as f:
    tree = p.parse(f.read())
    tree.check()

for filename in sys.argv[1:]:
    try:
        with open(filename) as f:
            tree = p.parse(f.read())
            tree.check()
            print(f"{filename}: семантических ошибок не найдено")
    except pe.Error as e:
        print(f"{filename}: Ошибка {e.pos}: {e.message}")
