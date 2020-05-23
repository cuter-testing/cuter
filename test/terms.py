# -*- coding: utf-8 -*-

import sys
import re, parsimonious

def parse(s):
    ast = to_ast(s)
    return eval_ast(ast, {})[0]

def parse_with_vars(s, vs):
    ast = to_ast(s)
    return eval_ast(ast, vs)[0]

def to_ast(s):
    grammar = parsimonious.Grammar("""\
    term = assignments / boolean / function / atom / list / tuple / number / bitstring / string / var
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
    point = ("(" _ (term (_ "," _ term)*)? _ ")" _ when? _ "->" _ term) / ("(" _ any (_ "," _ any)* ")" _ "->" _ term)
    when = "when" _ var _ "=:=" _ var ("," _ var _ "=:=" _ var)*
    assignments = _ var _ "=" _ function (_ "," _ var _ "=" _ function)*
    var = ~"[0-9A-Z]+"
    any = "_"
    """)
    return grammar.parse(s)

def eval_ast(ast, vs):
    node = ast.expr_name
    ts = [y for x in [eval_ast(z, vs) for z in ast.children] for y in x]
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
        parts = list(map(int, ast.text.split(":")))
        bs = bin(parts[0])[2:]
        return ["0" * (parts[1] - len(bs)) + bs]
    elif node == "function":
        return [lambda x: call_fn(ts, x)]
    elif node == "point":
        ps = ts[:-2] if len(ts) >= 2 and is_when(ts[-2]) else ts[:-1]
        w = ts[-2][1] if len(ts) >= 2 and is_when(ts[-2]) else {}
        for i, p in enumerate(ps):
            if p in w:
                ps[i] = w[p]
        return [(tuple(ps), ts[-1])]
    elif node == "any":
        return ["_"]
    elif node == "var":
        if ast.text in vs:
            return [vs[ast.text]]
        return [ast.text]
    elif node == "when":
        return [("__when", dict(list(group(ts))))]
    elif node == "assignments":
        return [dict(list(group(ts)))]
    else:
        return ts

def group(l):
    for i in range(0, len(l), 2):
        val = l[i:i+2]
        if len(val) == 2:
          yield tuple(val)

def is_when(x):
    return isinstance(x, tuple) and len(x) == 2 and x[0] == "__when"

def eval_string(ast):
    return ast.text[1:-1].replace(r'\"', '"')

def get_otherwise(points):
    for pms, v in points:
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
