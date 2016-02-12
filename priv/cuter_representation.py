#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
from z3 import *
import cuter_common as cc

class Erlang:
  """
  Erlang's Type System.
  """
  def __init__(self):
    # Define the representation
    self.Term, self.List, self.Atom, self.BitStr = self.create_representation()

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

if __name__ == '__main__':
  import json
  test_encoder()
