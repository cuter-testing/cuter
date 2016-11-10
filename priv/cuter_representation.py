#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
from z3 import *
from collections import defaultdict
import cuter_env as cenv
import cuter_common as cc
import cuter_global as cglb
import cuter_logger as clg
from cuter_proto_erlang_term_pb2 import ErlangTerm

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
        self.atmTrue = decoder.decodeTerm(cc.mk_atom([116,114,117,101]))
        self.atmFalse = decoder.decodeTerm(cc.mk_atom([102,97,108,115,101]))

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
      Encoder from Z3 terms to ErlangTerm messages.
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

    def encodeTerm(self, t):
        return self.encode(t).SerializeToString()

    def toSymbolic(self, s):
        t = ErlangTerm()
        t.type = ErlangTerm.SYMBOLIC_VARIABLE
        t.value = s
        return t

    def encode(self, t):
        T = self.erl.Term
        if (is_true(simplify(T.is_int(t)))):
            return self.toInt(T.ival(t))
        elif (is_true(simplify(T.is_real(t)))):
            return self.toFloat(T.rval(t))
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

    def toInt(self, term):
        t = ErlangTerm()
        t.type = ErlangTerm.INTEGER
        t.value = str(simplify(term).as_long())
        return t

    def toFloat(self, term):
        s = simplify(term)
        t = ErlangTerm()
        t.type = ErlangTerm.FLOAT
        t.value = str(float(s.numerator_as_long()) / float(s.denominator_as_long()))
        return t

    def toList(self, term):
        L = self.erl.List
        s = simplify(term)
        subterms = []
        while (is_true(simplify(L.is_cons(s)))):
            hd = simplify(L.hd(s))
            s = simplify(L.tl(s))
            subterms.append(self.encode(hd))
        t = ErlangTerm()
        t.type = ErlangTerm.LIST
        t.subterms.extend(subterms)
        return t

    def toTuple(self, term):
        L = self.erl.List
        s = simplify(term)
        subterms = []
        while (is_true(simplify(L.is_cons(s)))):
            hd = simplify(L.hd(s))
            s = simplify(L.tl(s))
            subterms.append(self.encode(hd))
        t = ErlangTerm()
        t.type = ErlangTerm.TUPLE
        t.subterms.extend(subterms)
        return t

    def toAtom(self, term):
        A = self.erl.Atom
        s = simplify(term)
        chars = []
        while (is_true(simplify(A.is_acons(s)))):
            hd = simplify(A.ahd(s))
            s = simplify(A.atl(s))
            chars.append(simplify(hd).as_long())
        t = ErlangTerm()
        t.type = ErlangTerm.ATOM
        t.atom_chars.extend(chars)
        return t

    def toBitstring(self, n, term):
        sz = simplify(n).as_long()
        if sz < 0:
            sz = 0
        B = self.erl.BitStr
        s = simplify(term)
        bits = []
        while (is_true(simplify(B.is_bcons(s)))) and sz > 0:
            sz -= 1
            hd = simplify(B.bhd(s))
            s = simplify(B.btl(s))
            bits.append(True if simplify(hd).as_long() == 1 else False)
        if sz > 0:
            bits.extend([0] * sz)
        t = ErlangTerm()
        t.type = ErlangTerm.BITSTRING
        t.bits.extend(bits)
        return t

    def toFun(self, n):
        idx = simplify(n).as_long()
        # In case the solver randomly select a solution to be a fun
        # without fmap existing in the model
        arity = self.arity[idx]
        if self.fmap is None or self.fmap[idx] is None:
            return self.defaultFun(arity)
        defn = self.getArrayDecl(self.fmap[idx], self.erl.List).as_list()
        # Also prune keys with the wrong arity (mainly to genericFun specs adding an Exists axiom)
        kvs = [self.encodeKVPair(args, val) for [args, val] in defn[0:-1] if self.getActualArity(args) == arity]
        if len(kvs) == 0:
            default = self.encodeDefault(defn[-1])
            return cc.mk_const_fun(arity, default)
        else:
            default = cc.get_value_from_fun_entry(kvs[0])
        # Do not add the arity.
        # It will be deducted from the number of the arguments of the
        # first input.
        # return {"t": cc.JSON_TYPE_FUN, "v": kvs, "x": arity}
        return cc.mk_fun(arity, kvs, default)

    def getArrayDecl(self, asArray, DomType):
        m = self.model
        return m[simplify(simplify(asArray)[Const('%', DomType)]).decl()]

    def encodeKVPair(self, arg, value):
        T, L = self.erl.Term, self.erl.List
        earg = self.encode(T.lst(arg))
        v = self.encode(value)
        return cc.mk_fun_entry(earg.subterms, v)
        return self.encode(T.tpl(L.cons(T.lst(arg), L.cons(value, L.nil))))

    def encodeDefault(self, val):
        T, L = self.erl.Term, self.erl.List
        return self.encode(val)

    def defaultFun(self, arity):
        default = self.encodeDefault(self.erl.Term.int(0))
        return cc.mk_const_fun(arity, default)

    def getActualArity(self, t):
        L = self.erl.List
        if is_true(simplify(L.is_cons(t))):
            return 1 + self.getActualArity(simplify(L.tl(t)))
        else:
            return 0

class TermDecoder:
    """
      Decoder from ErlangTerm protobuf objects to Z3 terms.
    """

    def __init__(self, erl, env):
        self.erl = erl
        self.env = env
        self.aliasDb = {}

    def decodeTerm(self, t):
        return self.decode(t, t.shared)

    def decode(self, t, shared):
        opts = {
            ErlangTerm.INTEGER: self.decode_int,
            ErlangTerm.FLOAT: self.decode_float,
            ErlangTerm.LIST: self.decode_list,
            ErlangTerm.IMPROPER_LIST: self.decode_improper_list,
            ErlangTerm.TUPLE: self.decode_tuple,
            ErlangTerm.ATOM: self.decode_atom,
            ErlangTerm.BITSTRING: self.decode_bitstring,
            ErlangTerm.PID: self.decode_pid,
            ErlangTerm.REFERENCE: self.decode_ref,
            ErlangTerm.SUBTERM: self.decode_alias,
            ErlangTerm.SYMBOLIC_VARIABLE: self.decode_symbolic
        }
        return opts[t.type](t, shared)

    def decode_symbolic(self, t, shared):
        s = self.env.lookup(t.value)
        assert s is not None, "Symbolic Variable lookup"
        return s

    def decode_alias(self, t, shared):
        l = t.value
        if l in self.aliasDb:
            return self.aliasDb[l]
        else:
            x = self.decode(shared[l], shared)
            self.aliasDb[l] = x
            return x

    def decode_int(self, t, shared):
        return self.erl.Term.int(int(t.value))

    def decode_float(self, t, shared):
        return self.erl.Term.real(float(t.value))

    def decode_list(self, t, shared):
        erl = self.erl
        tl = erl.List.nil
        for v in reversed(t.subterms):
            enc_v = self.decode(v, shared)
            tl = erl.List.cons(enc_v, tl)
        return erl.Term.lst(tl)

    def decode_tuple(self, t, shared):
        erl = self.erl
        tl = erl.List.nil
        for v in reversed(t.subterms):
            enc_v = self.decode(v, shared)
            tl = erl.List.cons(enc_v, tl)
        return erl.Term.tpl(tl)

    def decode_atom(self, t, shared):
        erl = self.erl
        tl = erl.Atom.anil
        for v in reversed(t.atom_chars):
            tl = erl.Atom.acons(v, tl)
        return erl.Term.atm(tl)

    def decode_bitstring(self, t, shared):
        erl = self.erl
        tl = erl.BitStr.bnil
        sz = len(t.bits)
        for v in reversed(t.bits):
            b = BitVecVal(1 if v else 0, 1)
            tl = erl.BitStr.bcons(b, tl)
        return erl.Term.bin(sz, tl)

    def decode_improper_list(self, t, shared):
        # TODO Propery decode improper lists once they are supported.
        return self.erl.Term.int(42)

    def decode_pid(self, t, shared):
        # TODO Propery decode PIDs once they are supported.
        return self.erl.Term.int(42)

    def decode_ref(self, t, shared):
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
            cc.mk_int(4242424242)
        ),
        ( # 3.14159
            T.real(3.14159),
            cc.mk_float(3.14159)
        ),
        ( # foo
            T.atm(A.acons(102,A.acons(111,A.acons(111,A.anil)))),
            cc.mk_atom([102,111,111])
        ),
        ( # [42, 3.14]
            T.lst(L.cons(T.int(42),L.cons(T.real(3.14),L.nil))),
            cc.mk_list([cc.mk_int(42), cc.mk_float(3.14)])
        ),
        ( # {foo, 42}
            T.tpl(L.cons(T.atm(A.acons(102,A.acons(111,A.acons(111,A.anil)))),L.cons(T.int(42),L.nil))),
            cc.mk_tuple([cc.mk_atom([102,111,111]), cc.mk_int(42)])
        ),
        ( # <<5:3>>
            T.bin(3, B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(0,1),B.bcons(BitVecVal(1,1),B.bnil)))),
            cc.mk_bitstring([True,False,True])
        ),
        ( # <<5:3>>
            T.bin(3, B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(0,1),B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(1,1),B.bnil))))),
            cc.mk_bitstring([True,False,True])
        ),
        ( # <<4:3>>
            T.bin(3, B.bcons(BitVecVal(1,1),B.bcons(BitVecVal(0,1),B.bnil))),
            cc.mk_bitstring([True,False,False])
        )
    ]
    for x, y in terms:
        z = erl.encode(x, None)
        assert z == y, "Encoded {} is not {} but {}".format(x, y, z)

def test_decoder_simple():
    erl = Erlang()
    env = cenv.Env()
    T, L, A, B = erl.Term, erl.List, erl.Atom, erl.BitStr
    # Create the term with shared subterms.
    tal = cc.mk_alias("0.0.0.42")
    x = cc.mk_tuple([tal, tal])
    xv = x.shared["0.0.0.42"]
    xv.type = ErlangTerm.LIST
    xv.subterms.extend([cc.mk_int(1)])
    terms = [
        ( # 42
            cc.mk_int(42),
            T.int(42)
        ),
        ( # 42.42
            cc.mk_float(42.42),
            T.real(42.42)
        ),
        ( # ok
            cc.mk_atom([111,107]),
            T.atm(A.acons(111,A.acons(107,A.anil)))
        ),
        ( # [1,2]
            cc.mk_list([cc.mk_int(1), cc.mk_int(2)]),
            T.lst(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {1,2}
            cc.mk_tuple([cc.mk_int(1), cc.mk_int(2)]),
            T.tpl(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {[1],[1]}
            x,
            T.tpl (L.cons(T.lst(L.cons(T.int(1),L.nil)),L.cons(T.lst(L.cons(T.int(1),L.nil)),L.nil)))
        ),
        ( # <<1:2>>
            cc.mk_bitstring([False, True]),
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
    # Create the term with shared subterms.
    tal = cc.mk_alias("0.0.0.42")
    x = cc.mk_tuple([tal, tal])
    xv = x.shared["0.0.0.42"]
    xv.type = ErlangTerm.LIST
    xv.subterms.extend([cc.mk_symb(s1)])
    terms = [
        ( # [1,2]
            cc.mk_list([cc.mk_symb(s1), cc.mk_symb(s2)]),
            T.lst(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {1,2}
            cc.mk_tuple([cc.mk_int(1), cc.mk_symb(s2)]),
            T.tpl(L.cons(T.int(1),L.cons(T.int(2),L.nil)))
        ),
        ( # {[1],[1]}
            x,
            T.tpl(L.cons(T.lst(L.cons(T.int(1),L.nil)),L.cons(T.lst(L.cons(T.int(1),L.nil)),L.nil)))
        ),
    ]
    decode_and_check(erl, env, terms)

def decode_and_check(erl, env, terms):
    for x, y in terms:
        z = TermDecoder(erl, env).decodeTerm(x)
        s = Solver()
        s.add(z == y)
        assert s.check() == sat, "Decoded {} is not {} but {}".format(x, y, z)

def compare_solutions(solExpected, solFound):
    deep_compare(solExpected, solFound)
    deep_compare(solFound, solExpected)

def deep_compare(d1, d2):
    assert type(d1) == type(d2)
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
    f_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([cc.mk_int(3)],  cc.mk_int(42)),
        cc.mk_fun_entry([cc.mk_int(10)], cc.mk_int(17)),
    ], cc.mk_int(42))
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
    f_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([x_sol], cc.mk_int(42)),
        cc.mk_fun_entry([y_sol], cc.mk_int(10))
    ], cc.mk_int(42))
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
        T.is_int(t1),
        T.is_int(t2),
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
    f_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([x_sol],        cc.mk_int(4)),
        cc.mk_fun_entry([cc.mk_int(5)], cc.mk_int(17)),
        cc.mk_fun_entry([cc.mk_int(4)], cc.mk_int(42)),
        cc.mk_fun_entry([y_sol],        cc.mk_int(5))
    ], cc.mk_int(4))
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
    t1_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([y_sol], cc.mk_int(42))
    ], cc.mk_int(42))
    f_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([x_sol], t1_exp)
    ], t1_exp)
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
    f_exp = cc.mk_fun(3, [
        cc.mk_fun_entry([z_sol, y_sol, x_sol], cc.mk_int(17)),
        cc.mk_fun_entry([x_sol, y_sol, z_sol], cc.mk_int(42))
    ], cc.mk_int(17))
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
    t2_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([cc.mk_int(42)], cc.mk_int(42))
    ], cc.mk_int(42))
    t1_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([cc.mk_int(42)], t2_exp)
    ], t2_exp)
    f_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([cc.mk_int(42)], t1_exp)
    ], t1_exp)
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
        T.is_int(h1),
        l1 == T.lst( L.tl( T.lval(l) ) ),
        t1 == fmap( T.fval(f) )[ L.cons(h1, L.cons(T.int(0), L.nil)) ],
        T.is_int(t1),
        # 2nd element
        L.is_cons( T.lval(l1) ),
        h2 == L.hd( T.lval(l1) ),
        T.is_int(h2),
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
    f_exp = cc.mk_fun(2, [
        cc.mk_fun_entry([l_sol.subterms[0], cc.mk_int(0)], cc.mk_int(4)),
        cc.mk_fun_entry([l_sol.subterms[1], cc.mk_int(4)], cc.mk_int(42))
    ], cc.mk_int(4))
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
    f_exp = cc.mk_fun(1, [
        cc.mk_fun_entry([l_sol.subterms[1]], encoder.encode(atmFalse)),
        cc.mk_fun_entry([l_sol.subterms[0]], encoder.encode(atmFalse))
    ], encoder.encode(atmFalse))
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
