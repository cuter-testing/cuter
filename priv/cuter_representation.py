#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
from z3 import *
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

  def encode(self, z3Term):
    """
    Encode a Z3 term to an Erlang JSON term.
    """
    encoder = TermEncoder(self)
    return encoder.encode(z3Term)

  def encodeSymbolic(self, symbString):
    """
    Encode a symbolic parameter.
    """
    encoder = TermEncoder(self)
    return encoder.toSymbolic(symbString)

class TermEncoder:
  """
    Encoder from Z3 terms to Erlang terms (in JSON representation).
  """
  def __init__(self, erl):
    self.erl = erl

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


# #############################################################################
# Unit Tests
# #############################################################################

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
    z = erl.encode(x)
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

if __name__ == '__main__':
  import json
  cglb.init()
  test_encoder()
  test_decoder_simple()
  test_decoder_complex()
