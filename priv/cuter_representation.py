#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
from z3 import *
from collections import defaultdict
import cuter_env as cenv
import cuter_common as cc
import cuter_global as cglb

class Erlang:
    """
    Erlang's Type System.
    """

    def __init__(self):
        # Define the representation
        self.Term, self.List, self.Atom, self.BitStr = self.create_representation()
        self.fmap = Function('fmap', IntSort(), ArraySort(self.List, self.Term))
        self.arity = Function('arity', IntSort(), IntSort())
        # Define the boolean values.
        decoder = TermDecoder(self, cenv.Env())
        self.atmTrue = decoder.decode(json.loads("{\"t\": 3, \"v\": [116,114,117,101]}"), {})
        self.atmFalse = decoder.decode(json.loads("{\"t\": 3, \"v\": [102,97,108,115,101]}"), {})

    def create_representation(self):
        Term = Datatype('Term')
        List = Datatype('List')
        Tuple = Datatype('Tuple')
        Atom = Datatype('Atom')
        BitStr = Datatype('BitStr')
        # Term
        Term.declare('int', ('ival', IntSort()))
        Term.declare('real', ('rval', RealSort()))
        Term.declare('lst', ('lval', List))
        Term.declare('tpl', ('tval', List))
        Term.declare('atm', ('aval', Atom))
        Term.declare('bin', ('bsz', IntSort()), ('bval', BitStr))
        Term.declare('fun', ('fval', IntSort()))
        # List
        List.declare('nil')
        List.declare('cons', ('hd', Term), ('tl', List))
        # Atom
        Atom.declare('anil')
        Atom.declare('acons', ('ahd', IntSort()), ('atl', Atom))
        # Bitstring
        BitStr.declare('bnil')
        BitStr.declare('bcons', ('bhd', BitVecSort(1)), ('btl', BitStr))
        # Return Datatypes
        return CreateDatatypes(Term, List, Atom, BitStr)

    def encode(self, z3Term, model):
        """
        Encode a Z3 term to an Erlang JSON term.
        """
        encoder = TermEncoder(self, model, self.fmap, self.arity)
        return encoder.encode(z3Term)

    def encodeSymbolic(self, symbString):
        """
        Encode a symbolic parameter.
        """
        encoder = TermEncoder(self)
        return encoder.toSymbolic(symbString)


# =============================================================================
# Encode / Decode terms.
# =============================================================================

class TermEncoder:
    """
      Encoder from Z3 terms to Erlang terms (in JSON representation).
    """

    def __init__(self, erl, model = None, fmap = None, arity = None):
        self.erl = erl
        self.model = model
        self.arity = self.parseArity(model, arity)
        self.fmap = self.parseFmap(model, fmap)

    def parseArity(self, model, handle):
        if model is None or handle is None or model[handle] is None:
            return defaultdict(lambda: 1)
        defn = model[handle].as_list()
        arity = defaultdict(lambda: simplify(defn[-1]).as_long())
        for x in defn[:-1]:
            arity[simplify(x[0]).as_long()] = simplify(x[1]).as_long()
        return arity

    def parseFmap(self, model, handle):
        if model is None or handle is None or model[handle] is None:
            return None
        # The default value with be the default fun and not simplify(defn[-1])
        defn = model[handle].as_list()
        fmap = defaultdict(lambda: None)
        for x in defn[:-1]:
            fmap[simplify(x[0]).as_long()] = simplify(x[1])
        return fmap

    def toSymbolic(self, s):
        return {"s": s}

    def encode(self, t):
        T = self.erl.Term
        if (is_true(simplify(T.is_int(t)))):
            return self.toInt(T.ival(t))
        elif (is_true(simplify(T.is_real(t)))):
            return self.toReal(T.rval(t))
        elif (is_true(simplify(T.is_lst(t)))):
            return self.toList(T.lval(t))
        elif (is_true(simplify(T.is_tpl(t)))):
            return self.toTuple(T.tval(t))
        elif (is_true(simplify(T.is_atm(t)))):
            return self.toAtom(T.aval(t))
        elif (is_true(simplify(T.is_bin(t)))):
            return self.toBitstring(T.bsz(t), T.bval(t))
        elif (is_true(simplify(T.is_fun(t)))):
            return self.toFun(T.fval(t))
        else:
            raise Exception("Could not recognise the type of term: " + str(t))

    def toInt(self, t):
        return {"t": cc.JSON_TYPE_INT, "v": simplify(t).as_long()}

    def toReal(self, t):
        s = simplify(t)
        f = float(s.numerator_as_long()) / float(s.denominator_as_long())
        return {"t": cc.JSON_TYPE_FLOAT, "v": f}

    def toList(self, t):
        L = self.erl.List
        s = simplify(t)
        r = []
        while (is_true(simplify(L.is_cons(s)))):
            hd = simplify(L.hd(s))
            s = simplify(L.tl(s))
            r.append(self.encode(hd))
        return {"t": cc.JSON_TYPE_LIST, "v": r}

    def toTuple(self, t):
        L = self.erl.List
        s = simplify(t)
        r = []
        while (is_true(simplify(L.is_cons(s)))):
            hd = simplify(L.hd(s))
            s = simplify(L.tl(s))
            r.append(self.encode(hd))
        return {"t": cc.JSON_TYPE_TUPLE, "v": r}

    def toAtom(self, t):
        A = self.erl.Atom
        s = simplify(t)
        r = []
        while (is_true(simplify(A.is_acons(s)))):
            hd = simplify(A.ahd(s))
            s = simplify(A.atl(s))
            r.append(simplify(hd).as_long())
        return {"t": cc.JSON_TYPE_ATOM, "v": r}

    def toBitstring(self, n, t):
        sz = simplify(n).as_long()
        if sz < 0:
            sz = 0
        B = self.erl.BitStr
        s = simplify(t)
        r = []
        while (is_true(simplify(B.is_bcons(s)))) and sz > 0:
            sz -= 1
            hd = simplify(B.bhd(s))
            s = simplify(B.btl(s))
            r.append(simplify(hd).as_long())
        if sz > 0:
            r.extend([0] * sz)
        return {"t": cc.JSON_TYPE_BITSTRING, "v": r}

    def toFun(self, n):
        idx = simplify(n).as_long()
        # In case the solver randomly select a solution to be a fun
        # without fmap existing in the model
        arity = self.arity[idx]
        if self.fmap is None or self.fmap[idx] is None:
            return self.defaultFun(arity)
        defn = self.getArrayDecl(self.fmap[idx], self.erl.List).as_list()
        # Do not add the defaut value.
        # It will be selected as the value of the first input.
        # TODO What will happen if the arity is 0?
        # default = self.encodeDefault(defn[-1])
        # Also prune keys with the wrong arity (mainly to genericFun specs adding an Exists axiom)
        kvs = [self.encodeKVPair(args, val) for [args, val] in defn[0:-1] if self.getActualArity(args) == arity]
        if len(kvs) == 0:
            default = self.encodeDefault(defn[-1])
            return {"t": cc.JSON_TYPE_FUN, "v": [default], "x": arity}
        # Do not add the arity.
        # It will be deducted from the number of the arguments of the
        # first input.
        # return {"t": cc.JSON_TYPE_FUN, "v": kvs, "x": arity}
        return {"t": cc.JSON_TYPE_FUN, "v": kvs}

    def getArrayDecl(self, asArray, DomType):
        m = self.model
        return m[simplify(simplify(asArray)[Const('%', DomType)]).decl()]

    def encodeKVPair(self, arg, value):
        T, L = self.erl.Term, self.erl.List
        return self.encode(T.tpl(L.cons(T.lst(arg), L.cons(value, L.nil))))

    def encodeDefault(self, val):
        T, L = self.erl.Term, self.erl.List
        return self.encode(T.tpl(L.cons(val, L.nil)))

    def defaultFun(self, arity):
        default = self.encodeDefault(self.erl.Term.int(0))
        return {"t": cc.JSON_TYPE_FUN, "v": [default], "x": arity}

    def getActualArity(self, t):
        L = self.erl.List
        if is_true(simplify(L.is_cons(t))):
            return 1 + self.getActualArity(simplify(L.tl(t)))
        else:
            return 0

class TermDecoder:
    """
      Decoder from Erlang terms (in JSON representation) to Z3 terms.
    """

    def __init__(self, erl, env):
        self.erl = erl
        self.env = env
        self.aliasDb = {}

    def decode(self, t, dct):
        if "s" in t:
            x = self.env.lookup(t["s"])
            assert x is not None, "Symbolic Variable lookup"
            return x
        if "l" in t:
            l = t["l"]
            if l in self.aliasDb:
                return self.aliasDb[l]
            else:
                x = self.decode(dct[l], dct)
                self.aliasDb[l] = x
                return x
        else:
            opts = {
                cc.JSON_TYPE_INT: self.decode_int,
                cc.JSON_TYPE_FLOAT: self.decode_float,
                cc.JSON_TYPE_LIST: self.decode_list,
                cc.JSON_TYPE_TUPLE: self.decode_tuple,
                cc.JSON_TYPE_ATOM: self.decode_atom,
                cc.JSON_TYPE_BITSTRING: self.decode_bitstring,
                cc.JSON_TYPE_PID: self.decode_pid,
                cc.JSON_TYPE_REF: self.decode_ref
            }
            return opts[t["t"]](t["v"], dct)

    def decode_int(self, v, dct):
        return self.erl.Term.int(v)

    def decode_float(self, v, dct):
        return self.erl.Term.real(v)

    def decode_list(self, v, dct):
        erl = self.erl
        v.reverse()
        t = erl.List.nil
        while v != []:
            hd, v = v[0], v[1:]
            enc_hd = self.decode(hd, dct)
            t = erl.List.cons(enc_hd, t)
        return erl.Term.lst(t)

    def decode_tuple(self, v, dct):
        erl = self.erl
        v.reverse()
        t = erl.List.nil
        while v != []:
            hd, v = v[0], v[1:]
            enc_hd = self.decode(hd, dct)
            t = erl.List.cons(enc_hd, t)
        return erl.Term.tpl(t)

    def decode_atom(self, v, dct):
        erl = self.erl
        v.reverse()
        t = erl.Atom.anil
        while v != []:
            hd, v = v[0], v[1:]
            t = erl.Atom.acons(hd, t)
        return erl.Term.atm(t)

    def decode_bitstring(self, v, dct):
        erl = self.erl
        v.reverse()
        t = erl.BitStr.bnil
        sz = len(v)
        while v != []:
            hd, v = BitVecVal(v[0], 1), v[1:]
            t = erl.BitStr.bcons(hd, t)
        return erl.Term.bin(sz, t)

    def decode_pid(self, v, dct):
        # TODO Propery decode PIDs once they are supported.
        return self.erl.Term.int(42)

    def decode_ref(self, v, dct):
        # TODO Propery decode references once they are supported.
        return self.erl.Term.int(42)


# =============================================================================
# Unit Tests
# =============================================================================

def test_encoder():
    erl = Erlang()
    T, L, A, B = erl.Term, erl.List, erl.Atom, erl.BitStr
    terms = [
        ( # 4242424242
            T.int(4242424242),
            {"t":cc.JSON_TYPE_INT,"v":4242424242}
        ),
        ( # 3.14159
            T.real(3.14159),
            {"t":cc.JSON_TYPE_FLOAT,"v":3.14159},
        ),
        ( # foo
            T.atm(A.acons(102,A.acons(111,A.acons(111,A.anil)))),
            {"t":cc.JSON_TYPE_ATOM,"v":[102,111,111]}
        ),
        ( # [42, 3.14]
            T.lst(L.cons(T.int(42),L.cons(T.real(3.14),L.nil))),
            {"t":cc.JSON_TYPE_LIST,"v":[{"t":cc.JSON_TYPE_INT,"v":42},{"t":cc.JSON_TYPE_FLOAT,"v":3.14}]}
        ),
        ( # {foo, 42}
            T.tpl(L.cons(T.atm(A.acons(102,A.acons(111,A.acons(111,A.anil)))),L.cons(T.int(42),L.nil))),
            {"t":cc.JSON_TYPE_TUPLE,"v":[{"t":cc.JSON_TYPE_ATOM,"v":[102,111,111]},{"t":cc.JSON_TYPE_INT,"v":42}]}
        ),
        ( # <<5:3>>
            T.bin(3, B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(0,1),B.bcons(BitVecVal(1,1),B.bnil)))),
            {"t":cc.JSON_TYPE_BITSTRING,"v":[1,0,1]}
        ),
        ( # <<5:3>>
            T.bin(3, B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(0,1),B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(1,1),B.bnil))))),
            {"t":cc.JSON_TYPE_BITSTRING,"v":[1,0,1]}
        ),
        ( # <<4:3>>
            T.bin(3, B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(0,1),B.bnil))),
            {"t":cc.JSON_TYPE_BITSTRING,"v":[1,0,0]}
        )
    ]
    for x, y in terms:
        z = erl.encode(x, None)
        assert z == y, "Encoded {} is not {} but {}".format(x, y, z)

def test_decoder_simple():
    erl = Erlang()
    env = cenv.Env()
    T, L, A, B = erl.Term, erl.List, erl.Atom, erl.BitStr
    terms = [
        ( # 42
            {"t":cc.JSON_TYPE_INT,"v":42},
            T.int(42)
        ),
        ( # 42.42
            {"t":cc.JSON_TYPE_FLOAT,"v":42.42},
            T.real(42.42)
        ),
        ( # ok
            {"t":cc.JSON_TYPE_ATOM,"v":[111,107]},
            T.atm(A.acons(111,A.acons(107,A.anil)))
        ),
        ( # [1,2]
            {"t":cc.JSON_TYPE_LIST,"v":[{"t":cc.JSON_TYPE_INT,"v":1},{"t":cc.JSON_TYPE_INT,"v":2}]},
            T.lst(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {1,2}
            {"t":cc.JSON_TYPE_TUPLE,"v":[{"t":cc.JSON_TYPE_INT,"v":1},{"t":cc.JSON_TYPE_INT,"v":2}]},
            T.tpl(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {[1],[1]}
            {"d":{"0.0.0.46":{"t":cc.JSON_TYPE_LIST,"v":[{"t":cc.JSON_TYPE_INT,"v":1}]}},"t":cc.JSON_TYPE_LIST,"v":[{"l":"0.0.0.46"},{"l":"0.0.0.46"}]},
            T.lst(L.cons(T.lst(L.cons(T.int(1),L.nil)),L.cons(T.lst(L.cons(T.int(1),L.nil)),L.nil)))
        ),
        ( # <<1:2>>
            {"t":cc.JSON_TYPE_BITSTRING,"v":[0,1]},
            T.bin(2, B.bcons(BitVecVal(0,1),B.bcons(BitVecVal(1,1),B.bnil)))
        )
    ]
    decode_and_check(erl, env, terms)

def test_decoder_complex():
    erl = Erlang()
    T, L = erl.Term, erl.List
    s1, s2 = "0.0.0.39316", "0.0.0.39317"
    env = cenv.Env()
    env.bind(s1, T.int(1))
    env.bind(s2, T.int(2))
    terms = [
        ( # [1,2]
            {"t":cc.JSON_TYPE_LIST,"v":[{"s":s1},{"s":s2}]},
            T.lst(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {1,2}
            {"t":cc.JSON_TYPE_TUPLE,"v":[{"t":cc.JSON_TYPE_INT,"v":1},{"s":s2}]},
            T.tpl(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {[1],[1]}
            {"d":{"0.0.0.46":{"t":cc.JSON_TYPE_LIST,"v":[{"s":s1}]}},"t":cc.JSON_TYPE_LIST,"v":[{"l":"0.0.0.46"},{"l":"0.0.0.46"}]},
            T.lst(L.cons(T.lst(L.cons(T.int(1),L.nil)),L.cons(T.lst(L.cons(T.int(1),L.nil)),L.nil)))
        ),
    ]
    decode_and_check(erl, env, terms)

def decode_and_check(erl, env, terms):
    for x, y in terms:
        z = TermDecoder(erl, env).decode(x, x["d"] if "d" in x else {})
        s = Solver()
        s.add(z == y)
        assert s.check() == sat, "Decoded {} is not {} but {}".format(x, y, z)

def mk_int(i):
    return {"t": cc.JSON_TYPE_INT, "v": i}

def mk_list(xs):
    return {"t": cc.JSON_TYPE_LIST, "v": xs}

def mk_tuple(xs):
    return {"t": cc.JSON_TYPE_TUPLE, "v": xs}

def mk_fun(xs, ar):
    # Ignore the arity.
    # return {"t": cc.JSON_TYPE_FUN, "v": xs, "x": ar}
    return {"t": cc.JSON_TYPE_FUN, "v": xs}

def compare_solutions(solExpected, solFound):
    deep_compare(solExpected, solFound)
    deep_compare(solFound, solExpected)

def deep_compare(d1, d2):
    assert type(d1) == type(d2)
    if type(d1) == list:
        [deep_compare(e1, e2) for e1, e2 in zip(sorted(d1), sorted(d2))]
    elif type(d1) == dict:
        for k in d1.keys():
            assert d2.has_key(k)
            deep_compare(d1[k], d2[k])
    else:
        assert d1 == d2

def fun_scenario1():
    """
    Scenario 1
    ----------
    ERLANG CODE
      -spec f1(fun((integer()) -> integer())) -> ok.
      f1(F) ->
        case F(3) of
          42 ->
            case F(10) of
              17 -> error(bug);
              _ -> ok
            end;
          _ -> ok
        end.
    TRACE
      is_fun(f, 1)
      f(3) = 42
      f(10) = 17
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    f = Const('f', T)
    slv.add([
        T.is_fun(f),
        arity( T.fval(f) ) == 1,
        fmap( T.fval(f) )[ L.cons(T.int(3), L.nil) ] == T.int(42),
        fmap( T.fval(f) )[ L.cons(T.int(10), L.nil) ] == T.int(17)
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    f_sol = encoder.encode(m[f])
    # Create the result
    f_exp = mk_fun([
        mk_tuple([ mk_list([mk_int(3)]),  mk_int(42) ]),
        mk_tuple([ mk_list([mk_int(10)]), mk_int(17) ])
    ], 1)
    compare_solutions(f_exp, f_sol)

def fun_scenario2():
    """
    Scenario 2
    ----------
    ERLANG CODE
      -spec f2(fun((integer()) -> integer()), integer(), integer()) -> ok.
      f2(F, X, Y) ->
        case F(X) of
          42 ->
            case F(Y) of
              17 -> error(bug);
              _ -> ok
            end;
          _ -> ok
        end.
    TRACE
      is_int(x)
      is_int(y)
      is_fun(f, 1)
      f(x) = 42
      f(y) = 10
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    x, y, f = Consts('x, y, f', T)
    slv.add([
        T.is_fun(f),
        T.is_int(x),
        T.is_int(y),
        arity( T.fval(y) ) == 1,
        fmap( T.fval(f) )[ L.cons(x, L.nil) ] == T.int(42),
        fmap( T.fval(f) )[ L.cons(y, L.nil) ] == T.int(10)
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    x_sol, y_sol, f_sol = [encoder.encode(m[v]) for v in [x, y, f]]
    # Create the result
    f_exp = mk_fun([
        mk_tuple([ mk_list([x_sol]), mk_int(42) ]),
        mk_tuple([ mk_list([y_sol]), mk_int(10) ])
    ], 1)
    compare_solutions(f_exp, f_sol)

def fun_scenario3():
    """
    Scenario 3
    ----------
    ERLANG CODE
      -spec f3(fun((integer()) -> integer()), integer(), integer()) -> ok.
      f3(F, X, Y) ->
        case double(F, X) of
          42 ->
            case double(F, Y) of
             17 -> error(bug);
             _ -> ok
            end;
          _ -> ok
        end.
    TRACE
      is_int(x)
      is_int(y)
      is_fun(f, 1)
      t1 = f(x)
      f(t1) = 42
      t2 = f(y)
      f(t2) = 17
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    x, y, f, t1, t2 = Consts('x, y, f, t1, t2', T)
    slv.add([
        T.is_int(x),
        T.is_int(y),
        T.is_fun(f),
        arity( T.fval(f) ) == 1,
        fmap( T.fval(f) )[ L.cons(x, L.nil) ] == t1,
        fmap( T.fval(f) )[ L.cons(t1, L.nil) ] == T.int(42),
        fmap( T.fval(f) )[ L.cons(y, L.nil) ] == t2,
        fmap( T.fval(f) )[ L.cons(t2, L.nil) ] == T.int(17)
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    x_sol, y_sol, f_sol = [encoder.encode(m[v]) for v in [x, y, f]]
    # Create the result
    f_exp = mk_fun([
        mk_tuple([ mk_list([x_sol]),     mk_int(4)  ]),
        mk_tuple([ mk_list([mk_int(4)]), mk_int(42) ]),
        mk_tuple([ mk_list([y_sol]),     mk_int(5)  ]),
        mk_tuple([ mk_list([mk_int(5)]), mk_int(17) ])
    ], 1)
    compare_solutions(f_exp, f_sol)

def fun_scenario4():
    """
    Scenario 4
    ----------
    ERLANG CODE
      -spec f4(fun((integer()) -> integer()), integer(), integer()) -> ok.
      f4(F, X, Y) ->
        Z = F(X),
        case Z(Y) of
          42 -> error(bug);
          _ -> ok
        end.
    TRACE
      is_int(x)
      is_int(y)
      is_fun(f, 1)
      t1 = f(x)
      t1(y) = 42
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    x, y, f, t1 = Consts('x, y, f, t1', T)
    slv.add([
        T.is_int(x),
        T.is_int(y),
        T.is_fun(f),
        arity( T.fval(f) ) == 1,
        fmap( T.fval(f) )[ L.cons(x, L.nil) ] == t1,
        T.is_fun(t1),
        arity( T.fval(t1) ) == 1,
        fmap( T.fval(t1) )[ L.cons(y, L.nil) ] == T.int(42),
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    x_sol, y_sol, f_sol = [encoder.encode(m[v]) for v in [x, y, f]]
    # Create the result
    t1_exp = mk_fun([
        mk_tuple([ mk_list([y_sol]), mk_int(42) ])
    ], 1)
    f_exp = mk_fun([
        mk_tuple([ mk_list([x_sol]), t1_exp  ])
    ], 1)
    compare_solutions(f_exp, f_sol)

def fun_scenario5():
    """
    Scenario 5
    ----------
    ERLANG CODE
      -spec f5(fun((integer()) -> integer()), integer(), integer(), integer()) -> ok.
      f5(F, X, Y, Z) ->
        case F(X, Y, Z) of
          42 ->
            case F(Z, Y, X) of
              17 -> error(bug);
              _ -> ok
            end;
          _ -> ok
        end.
    TRACE
      is_int(x)
      is_int(y)
      is_int(z)
      is_fun(f, 3)
      f(x, y, z) = 42
      f(z, y, x) = 17
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    x, y, z, f = Consts('x, y, z, f', T)
    slv.add([
        T.is_int(x),
        T.is_int(y),
        T.is_int(z),
        T.is_fun(f),
        arity( T.fval(f) ) == 3,
        fmap( T.fval(f) )[ L.cons(x, L.cons(y, L.cons(z, L.nil))) ] == T.int(42),
        fmap( T.fval(f) )[ L.cons(z, L.cons(y, L.cons(x, L.nil))) ] == T.int(17),
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    x_sol, y_sol, z_sol, f_sol = [encoder.encode(m[v]) for v in [x, y, z, f]]
    # Create the result
    f_exp = mk_fun([
        mk_tuple([ mk_list([x_sol, y_sol, z_sol]), mk_int(42) ]),
        mk_tuple([ mk_list([z_sol, y_sol, x_sol]), mk_int(17) ])
    ], 3)
    compare_solutions(f_exp, f_sol)

def fun_scenario6():
    """
    Scenario 6
    ----------
    ERLANG CODE
      -spec f6(any()) -> any().
      f6(X) when is_function(X, 1) -> f6(X(42));
      f6(X) when X =/= 42 -> X.
    TRACE (with 3 fun applications)
      is_fun(f, 1)
      t1 = f(42)
      t2 = t1(42)
      t3 = t2(42)
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    f, t1, t2 = Consts('f, t1, t2', T)
    slv.add([
        # 1st step.
        T.is_fun(f),
        arity( T.fval(f) ) == 1,
        fmap( T.fval(f) )[ L.cons(T.int(42), L.nil) ] == t1,
        # 2nd step.
        T.is_fun(t1),
        arity( T.fval(t1) ) == 1,
        fmap( T.fval(t1) )[ L.cons(T.int(42), L.nil) ] == t2,
        # 3rd step.
        T.is_fun(t2),
        arity( T.fval(t2) ) == 1,
        fmap( T.fval(t2) )[ L.cons(T.int(42), L.nil) ] == T.int(42),
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    f_sol = encoder.encode(m[f])
    # Create the result
    t2_exp = mk_fun([
        mk_tuple([ mk_list([mk_int(42)]), mk_int(42) ])
    ], 1)
    t1_exp = mk_fun([
        mk_tuple([ mk_list([mk_int(42)]), t2_exp ])
    ], 1)
    f_exp = mk_fun([
        mk_tuple([ mk_list([mk_int(42)]), t1_exp ])
    ], 1)
    compare_solutions(f_exp, f_sol)

def fun_scenario7():
    """
    Scenario 7
    ----------
    ERLANG CODE
      -spec f7(fun((integer(), integer()) -> integer()), [integer()]) -> integer().
      f7(F, L) when is_function(F, 2) ->
        case lists:foldl(F, 0, L) of
          42 -> error(bug);
          R -> R 
      end.
    TRACE
      is_fun(f, 2)
      is_lst(l)
      l = [h1 | l1]
      t1 = f(h1, 0)
      l1 = [h2 | l2]
      l2 = []
      f(h2, t1) = 42
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    f, l, h1, l1, h2, l2, t1, t2 = Consts('f, l, h1, l1, h2, l2, t1, t2', T)
    slv.add([
        # Init
        T.is_fun(f),
        arity( T.fval(f) ) == 2,
        T.is_lst(l),
        # 1st element
        L.is_cons( T.lval(l) ),
        h1 == L.hd( T.lval(l) ),
        l1 == T.lst( L.tl( T.lval(l) ) ),
        t1 == fmap( T.fval(f) )[ L.cons(h1, L.cons(T.int(0), L.nil)) ],
        # 2nd element
        L.is_cons( T.lval(l1) ),
        h2 == L.hd( T.lval(l1) ),
        l2 == T.lst( L.tl( T.lval(l1) ) ),
        t2 == fmap( T.fval(f) )[ L.cons(h2, L.cons(t1, L.nil)) ],
        # Result
        L.is_nil( T.lval(l2) ),
        t2 == T.int(42)
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    l_sol, f_sol = [encoder.encode(m[v]) for v in [l, f]]
    # Create the result
    f_exp = mk_fun([
        mk_tuple([ mk_list([l_sol["v"][0], mk_int(0)]), mk_int(4)  ]),
        mk_tuple([ mk_list([l_sol["v"][1], mk_int(4)]), mk_int(42) ])
    ], 2)
    compare_solutions(f_exp, f_sol)

def fun_scenario8():
    """
    Scenario 8
    ----------
    ERLANG CODE
      -spec f8(fun((integer()) -> integer()), [integer()]) -> integer().
      f8(F, L) when is_function(F, 1) ->
        L1 = lists:filter(F, L),
        hd(L1).
    TRACE
      is_fun(f, 1)
      is_lst(l)
      l = [h1 | l1]
      f(h1) = false
      l1 = [h2 | l2]
      f(h2) = false
      l2 = []
    """
    erl = Erlang()
    T, L, fmap, arity, atmFalse = erl.Term, erl.List, erl.fmap, erl.arity, erl.atmFalse
    # Create the model
    slv = Solver()
    f, l, h1, l1, h2, l2 = Consts('f, l, h1, l1, h2, l2', T)
    slv.add([
        # Init
        T.is_fun(f),
        arity( T.fval(f) ) == 1,
        T.is_lst(l),
        # 1st element
        L.is_cons( T.lval(l) ),
        h1 == L.hd( T.lval(l) ),
        l1 == T.lst( L.tl( T.lval(l) ) ),
        atmFalse == fmap( T.fval(f) )[ L.cons(h1, L.nil) ],
        # 2nd element
        L.is_cons( T.lval(l1) ),
        h2 == L.hd( T.lval(l1) ),
        l2 == T.lst( L.tl( T.lval(l1) ) ),
        atmFalse == fmap( T.fval(f) )[ L.cons(h2, L.nil) ],
        # Result
        L.is_nil( T.lval(l2) )
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    l_sol, f_sol = [encoder.encode(m[v]) for v in [l, f]]
    # Create the result
    f_exp = mk_fun([
        mk_tuple([ mk_list([l_sol["v"][0]]), encoder.encode(atmFalse) ]),
        mk_tuple([ mk_list([l_sol["v"][1]]), encoder.encode(atmFalse) ])
    ], 1)
    compare_solutions(f_exp, f_sol)


def fun_scenario9():
    """
    Scenario 9
    ----------
    ERLANG CODE
    TRACE
    """
    erl = Erlang()
    T, L, fmap, arity = erl.Term, erl.List, erl.fmap, erl.arity
    # Create the model
    slv = Solver()
    x, f = Consts('x, f', T)
    slv.add([
        T.is_int(x),
        T.is_fun(f)
    ])
    # Solve the model
    chk = slv.check()
    assert chk == sat, "Model in unsatisfiable"
    m = slv.model()
    encoder = TermEncoder(erl, m, fmap, arity)
    x_sol, f_sol = [encoder.encode(m[v]) for v in [x, f]]
    # Create the result
    f_exp = encoder.defaultFun(1)
    compare_solutions(f_exp, f_sol)

def fun_scenarios():
    """
    Runs all the scenarios with funs.
    """
    fun_scenario1()
    fun_scenario2()
    fun_scenario3()
    fun_scenario4()
    fun_scenario5()
    fun_scenario6()
    fun_scenario7()
    fun_scenario8()
    fun_scenario9()

if __name__ == '__main__':
    import json
    cglb.init()
    test_encoder()
    test_decoder_simple()
    test_decoder_complex()
    fun_scenarios()
