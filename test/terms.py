#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import re, parsimonious

def parse(s):
    ast = to_ast(s)
    return eval_ast(ast)[0]

def to_ast(s):
    grammar = parsimonious.Grammar("""\
    term = boolean / function / atom / list / tuple / number / bitstring / string
    _ = ~"\s*"
    list = ("[" _ term (_ "," _ term)* _ "]") / ("[" _ "]")
    tuple = ("{" _ term (_ "," _ term)* _ "}") / ("{" _ "}")
    atom = ~"[a-z][0-9a-zA-Z_]*" / ("'" ~"[^']*" "'")
    number = ~"\-?[0-9]+(\.[0-9]+)?(e\-?[0-9]+)?"
    boolean = "true" / "false"
    bitstring = ("<<" _ (byte _ "," _)* byte _ ">>") / ("<<" _ (byte _ "," _)* bitexp  _ ">>") / ("<<" bitstr ">>")
    bitexp = byte ":" byte
    byte = ~"[0-9]+"
    string = '"' ~r'(\\\\"|[^"])*' '"'
    bitstr = '"' ~r'(\\\\"|[^"])*' '"'
    function = "fun" point (_ ";" _ point)* _ "end"
    point = ("(" _ (term (_ "," _ term)*)? _ ")" _ "->" _ term) / ("(" _ any (_ "," _ any)* ")" _ "->" _ term)
    any = "_"
    """)
    return grammar.parse(s)

def eval_ast(ast):
    node = ast.expr_name
    ts = [y for x in map(eval_ast, ast.children) for y in x]
    if node == "number":
        return [float(ast.text) if "." in ast.text else int(ast.text)]
    elif node == "boolean":
        return [ast.text == "true"]
    elif node == "atom":
        return [ast.text]
    elif node == "list":
        return [ts]
    elif node == "tuple":
        return [tuple(ts)]
    elif node == "string":
        return [eval_string(ast)]
    elif node == "bitstr":
        bs = [bin(ord(c))[2:] for c in eval_string(ast)]
        return ["0" * (8 - len(b)) + b for b in bs]
    elif node == "bitstring":
        return ["0b" + "".join(ts)]
    elif node == "byte":
        bs = bin(int(ast.text))[2:]
        return ["0" * (8 - len(bs)) + bs]
    elif node == "bitexp":
        parts = map(int, ast.text.split(":"))
        bs = bin(parts[0])[2:]
        return ["0" * (parts[1] - len(bs)) + bs]
    elif node == "function":
        return [lambda x: call_fn(ts, x)]
    elif node == "point":
        return [(tuple(ts[:-1]), ts[-1])]
    elif node == "any":
        return ["_"]
    else:
        return ts

def eval_string(ast):
    return ast.text[1:-1].replace(r'\"', '"')

def get_otherwise(points):
    for (pms, v) in points:
        if all(p == "_" or p == () for p in pms):
            return (pms, v)
    return (None, None)

def call_fn(points, arg):
    for p in points:
        if p[0] == arg:
            return p[1]
    return get_otherwise(points)[1]

if __name__ == "__main__":
    sys.stdout.write("{} ...".format(__file__))
    terms = [
        ("42", 42),
        ("[42]", [42]),
        ("[0, 5, 2]", [0, 5, 2]),
        ("<<14, 13, 12>>", "0b000011100000110100001100"),
        ("[<<0,42:7>>]", ["0b000000000101010"]),
        ("[<<42:6>>, 6]", ["0b101010", 6]),
        ("[[{[1,2],[{[[1],[2]],[3.14]},2]}]]", [[([1,2],[([[1],[2]],[3.14]),2])]]),
        ("[\"42\"]", ["42"]),
        ("[\"*\"]", ["*"]),
        ("[<<\"*\">>, 8]", ["0b00101010", 8]),
        ("[<<\"ok\">>]", ["0b0110111101101011"]),
    ]
    for s, t in terms:
        r = parse(s)
        assert r == t, "{} === {}".format(r, t)
    fns = [
        ("fun(3) -> 42 end", {(3,):42}),
        ("fun(3) -> 42; (42) -> 0 end", {(42,):0, (3,):42}),
        ("fun(3, ok) -> {17, 42} end", {(3,"ok"):(17,42)}),
        ("fun(3) -> 42; (10) -> 17; (_) -> 42 end", {(3,):42, (10,):17}),
        ("fun(1,2) -> 17; (_,_) -> 42 end", {(1,2):17}),
    ]
    for fn in fns:
        for x, v in fn[1].items():
            r = parse(fn[0])(x)
            assert r == v, "{} === {}".format(r, v)
    sys.stdout.write(" \033[01;32mok\033[00m\n")
    sys.stdout.flush
