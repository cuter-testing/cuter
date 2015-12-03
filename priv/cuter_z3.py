#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
from z3 import *
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc
import cuter_env as cenv
import cuter_types as ctp

# Set Z3Py params.
set_param(max_lines=1, max_width=1000000, max_depth=10000000, max_visited=1000000)
set_param('smt.bv.enable_int2bv', True)

class TermEncoder:
  def __init__(self, eZ3):
    self.eZ3 = eZ3
  
  def toSymbolic(self, s):
    return {"s": s}
  
  def fromZ3(self, t):
    T = self.eZ3.Term
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
    L = self.eZ3.List
    s = simplify(t)
    r = []
    while (is_true(simplify(L.is_cons(s)))):
      hd = simplify(L.hd(s))
      s = simplify(L.tl(s))
      r.append(self.fromZ3(hd))
    return {"t": cc.JSON_TYPE_LIST, "v": r}
  
  def toTuple(self, t):
    L = self.eZ3.List
    s = simplify(t)
    r = []
    while (is_true(simplify(L.is_cons(s)))):
      hd = simplify(L.hd(s))
      s = simplify(L.tl(s))
      r.append(self.fromZ3(hd))
    return {"t": cc.JSON_TYPE_TUPLE, "v": r}
  
  def toAtom(self, t):
    A = self.eZ3.Atom
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
    B = self.eZ3.BitStr
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
  def __init__(self, eZ3):
    self.eZ3 = eZ3
    self.env = {}
  
  def toZ3(self, t, dct):
    if "s" in t:
      x = self.eZ3.env.lookup(t["s"])
      assert x is not None, "Symbolic Variable lookup"
      return x
    if "l" in t:
      l = t["l"]
      if l in self.env:
        return self.env[l]
      else:
        x = self.toZ3(dct[l], dct)
        self.env[l] = x
        return x
    else:
      opts = {
        cc.JSON_TYPE_INT: self.int_toZ3,
        cc.JSON_TYPE_FLOAT: self.float_toZ3,
        cc.JSON_TYPE_LIST: self.list_toZ3,
        cc.JSON_TYPE_TUPLE: self.tuple_toZ3,
        cc.JSON_TYPE_ATOM: self.atom_toZ3,
        cc.JSON_TYPE_BITSTRING: self.bitstring_toZ3
      }
      return opts[t["t"]](t["v"], dct)
  
  def int_toZ3(self, v, dct):
    return self.eZ3.Term.int(v)
  
  def float_toZ3(self, v, dct):
    return self.eZ3.Term.real(v)
  
  def list_toZ3(self, v, dct):
    v.reverse()
    eZ3 = self.eZ3
    t = eZ3.List.nil
    while v != []:
      hd, v = v[0], v[1:]
      enc_hd = self.toZ3(hd, dct)
      t = eZ3.List.cons(enc_hd, t)
    return eZ3.Term.lst(t)
  
  def tuple_toZ3(self, v, dct):
    v.reverse()
    eZ3 = self.eZ3
    t = eZ3.List.nil
    while v != []:
      hd, v = v[0], v[1:]
      enc_hd = self.toZ3(hd, dct)
      t = eZ3.List.cons(enc_hd, t)
    return eZ3.Term.tpl(t)
  
  def atom_toZ3(self, v, dct):
    v.reverse()
    eZ3 = self.eZ3
    t = eZ3.Atom.anil
    while v != []:
      hd, v = v[0], v[1:]
      t = eZ3.Atom.acons(hd, t)
    return eZ3.Term.atm(t)

  def bitstring_toZ3(self, v, dct):
    v.reverse()
    eZ3 = self.eZ3
    t = eZ3.BitStr.bnil
    sz = len(v)
    while v != []:
      hd, v = BitVecVal(v[0], 1), v[1:]
      t = eZ3.BitStr.bcons(hd, t)
    return eZ3.Term.bin(sz, t)


class ErlangZ3:
  def __init__(self):
    self.Term, self.List, self.Atom, self.BitStr = self.erlang_type_system()
    self.env = cenv.Env()
    self.axs = []
    self.quantifier_axs = []
    self.slv = Then('simplify', 'normalize-bounds', 'solve-eqs', 'bit-blast', 'aig', 'qflra', 'qfnia', 'qfnra', 'qfbv',
                    'qfufnra', 'qflia', 'nlsat', 'qfnra-nlsat', 'qe', 'sat', 'smt').solver()
    self.slv.set('timeout', 10000)
    if cglb.__LISTS_INTERP__ == cglb.LISTS_FORALL_PATS:
      self.slv.set(mbqi=False)
    else:
      self.slv.set(mbqi=True)
    self.slv.set(auto_config=False)
    
    self.atmTrue = self.term_toZ3(json.loads("{\"t\": 3, \"v\": [116,114,117,101]}"))
    self.atmFalse = self.term_toZ3(json.loads("{\"t\": 3, \"v\": [102,97,108,115,101]}"))
    
    self.check = None
    self.model = None

  ## Define the Erlang Type System
  def erlang_type_system(*args):
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
  
  # Reset the solver
  def reset_solver(self):
    self.slv = Solver()
  
  # Add the axioms to the solver
  def add_axioms(self):
    self.slv.add(simplify(And(*self.quantifier_axs)))
    self.slv.add(simplify(And(*self.axs)))
  
  # Solve a Constraint Set
  def solve(self):
    self.check = self.slv.check()
    if self.check == sat:
      self.model = self.slv.model()
      return cc.SOLVER_STATUS_SAT
    elif self.check == unsat:
      return cc.SOLVER_STATUS_UNSAT
    elif self.check == unknown:
      clg.model_unknown(And(*self.quantifier_axs), And(*self.axs))
      return cc.SOLVER_STATUS_UNKNOWN
  
  # Fix a symbolic variable to a specific value
  def fix_parameter(self, p, v):
    x = self.term_toZ3(p)
    t = self.term_toZ3(v)
    self.slv.add(x == t)
  
  # Encode the resulting model to JSON
  def encode_model(self):
    m = []
    for p in self.env.params:
      m.append(self.encode_parameter(p))
    return m
  
  def encode_parameter(self, p):
    x = self.env.lookup(p)
    v = self.model[x]
    te = TermEncoder(self)
    symb = te.toSymbolic(p)
    if (v is None):
      return symb, {"t": cc.JSON_TYPE_ANY}
    else:
      return symb, te.fromZ3(v)
  
  def term_toZ3(self, jdata):
    td = TermDecoder(self)
    dct = jdata["d"] if ("d" in jdata) else {}
    return td.toZ3(jdata, dct)
  
  
  # ######################################################################
  # Load the recorded traces to Z3
  # ######################################################################
  
  def command_toZ3(self, tp, json_data, rev):
    opts_normal = {
      # Constraints
      cc.OP_GUARD_TRUE: self.guard_true_toZ3,
      cc.OP_GUARD_FALSE: self.guard_false_toZ3,
      cc.OP_MATCH_EQUAL_TRUE: self.match_equal_toZ3,
      cc.OP_MATCH_EQUAL_FALSE: self.match_not_equal_toZ3,
      cc.OP_TUPLE_SZ: self.tuple_sz_toZ3,
      cc.OP_TUPLE_NOT_SZ: self.tuple_not_sz_toZ3,
      cc.OP_TUPLE_NOT_TPL: self.tuple_not_tpl_toZ3,
      cc.OP_LIST_NON_EMPTY: self.list_nonempty_toZ3,
      cc.OP_LIST_EMPTY: self.list_empty_toZ3,
      cc.OP_LIST_NOT_LST: self.list_not_lst_toZ3,
      cc.OP_EMPTY_BITSTR: self.empty_bitstr_toZ3,
      cc.OP_NONEMPTY_BITSTR: self.nonempty_bitstr_toZ3,
      cc.OP_BITMATCH_CONST_TRUE: self.bitmatch_const_true_toZ3,
      cc.OP_BITMATCH_CONST_FALSE: self.bitmatch_const_false_toZ3,
      cc.OP_BITMATCH_VAR_TRUE: self.bitmatch_var_true_toZ3,
      cc.OP_BITMATCH_VAR_FALSE: self.bitmatch_var_false_toZ3,
      # Other important commands
      cc.OP_PARAMS: self.params_toZ3,
      cc.OP_SPEC: self.spec_toZ3,
      cc.OP_UNFOLD_TUPLE: self.unfold_tuple_toZ3,
      cc.OP_UNFOLD_LIST: self.unfold_list_toZ3,
      cc.OP_MAKE_BITSTR: self.make_bitstr_toZ3,
      cc.OP_CONCAT_SEGS: self.concat_segs_toZ3,
      # Erlang BIFs
      cc.OP_HD: self.hd_toZ3,
      cc.OP_TL: self.tl_toZ3,
      cc.OP_IS_INTEGER: self.is_integer_toZ3,
      cc.OP_IS_ATOM: self.is_atom_toZ3,
      cc.OP_IS_FLOAT: self.is_float_toZ3,
      cc.OP_IS_LIST: self.is_list_toZ3,
      cc.OP_IS_TUPLE: self.is_tuple_toZ3,
      cc.OP_IS_BOOLEAN: self.is_boolean_toZ3,
      cc.OP_IS_NUMBER: self.is_number_toZ3,
      cc.OP_PLUS: self.plus_toZ3,
      cc.OP_MINUS: self.minus_toZ3,
      cc.OP_TIMES: self.times_toZ3,
      cc.OP_RDIV: self.rdiv_toZ3,
      cc.OP_IDIV_NAT: self.idiv_nat_toZ3,
      cc.OP_REM_NAT: self.rem_nat_toZ3,
      cc.OP_UNARY: self.unary_toZ3,
      cc.OP_EQUAL: self.equal_toZ3,
      cc.OP_UNEQUAL: self.unequal_toZ3,
      cc.OP_FLOAT: self.float_toZ3,
      cc.OP_BOGUS: self.bogus_toZ3,
      cc.OP_ATOM_NIL: self.atom_nil_toZ3,
      cc.OP_ATOM_HEAD: self.atom_head_toZ3,
      cc.OP_ATOM_TAIL: self.atom_tail_toZ3,
      cc.OP_LIST_TO_TUPLE: self.list_to_tuple_toZ3,
      cc.OP_TUPLE_TO_LIST: self.tuple_to_list_toZ3,
      cc.OP_LT_INT: self.lt_integers_toZ3,
      cc.OP_LT_FLOAT: self.lt_floats_toZ3,
      cc.OP_CONS: self.cons_toZ3,
      cc.OP_TCONS: self.tcons_toZ3,
      cc.OP_POW: self.pow_toZ3,
      cc.OP_IS_BITSTRING: self.is_bitstring_toZ3,
    }
    
    opts_rev = {
      # Constraints
      cc.OP_GUARD_TRUE: self.guard_false_toZ3,
      cc.OP_GUARD_FALSE: self.guard_true_toZ3,
      cc.OP_MATCH_EQUAL_TRUE: self.match_not_equal_toZ3,
      cc.OP_MATCH_EQUAL_FALSE: self.match_equal_toZ3,
      cc.OP_TUPLE_SZ: self.tuple_sz_toZ3_RV,
      cc.OP_TUPLE_NOT_SZ: self.tuple_not_sz_toZ3_RV,
      cc.OP_TUPLE_NOT_TPL: self.tuple_not_tpl_toZ3_RV,
      cc.OP_LIST_NON_EMPTY: self.list_nonempty_toZ3_RV,
      cc.OP_LIST_EMPTY: self.list_empty_toZ3_RV,
      cc.OP_LIST_NOT_LST: self.list_not_lst_toZ3_RV,
      cc.OP_EMPTY_BITSTR: self.empty_bitstr_toZ3_RV,
      cc.OP_NONEMPTY_BITSTR: self.nonempty_bitstr_toZ3_RV,
      cc.OP_BITMATCH_CONST_TRUE: self.bitmatch_const_true_toZ3_RV,
      cc.OP_BITMATCH_CONST_FALSE: self.bitmatch_const_false_toZ3_RV,
      cc.OP_BITMATCH_VAR_TRUE: self.bitmatch_var_true_toZ3_RV,
      cc.OP_BITMATCH_VAR_FALSE: self.bitmatch_var_false_toZ3_RV,
      # Erlang BIFs
      cc.OP_HD: self.hd_toZ3_RV,
      cc.OP_TL: self.tl_toZ3_RV,
    }
    
    opts = opts_rev if rev else opts_normal
    opts[tp](*json_data["a"])
  
  # ----------------------------------------------------------------------
  # Constraints
  # ----------------------------------------------------------------------
  
  # Guard True
  def guard_true_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.append(t == self.atmTrue)
  
  # Guard False
  def guard_false_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.append(t == self.atmFalse)
  
  # NonEmpty List
  def list_nonempty_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.extend([
      self.Term.is_lst(t),
      self.List.is_cons(self.Term.lval(t))
    ])
  
  # Empty List
  def list_empty_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.extend([
      self.Term.is_lst(t),
      self.List.is_nil(self.Term.lval(t))
    ])
  
  # Not a List
  def list_not_lst_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_lst(t) == False)
  
  # Match Equal
  def match_equal_toZ3(self, term1, term2):
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(t1 == t2)
  
  # Match Not Equal
  def match_not_equal_toZ3(self, term1, term2):
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(t1 != t2)
  
  # Tuple of Size N
  def tuple_sz_toZ3(self, term, num):
    t = self.term_toZ3(term)
    nTerm = self.term_toZ3(num)
    n = int(str(simplify(self.Term.ival(nTerm)))) # Expect num to represent an Integer
    self.axs.append(self.Term.is_tpl(t))
    t = self.Term.tval(t)
    for i in range(0, n):
      self.axs.append(self.List.is_cons(t))
      t = self.List.tl(t)
    self.axs.append(t == self.List.nil)
  
  # Tuple of Not Size N
  def tuple_not_sz_toZ3(self, term, num):
    t = self.term_toZ3(term)
    nTerm = self.term_toZ3(num)
    n = int(str(simplify(self.Term.ival(nTerm)))) # Expect num to represent an Integer
    self.axs.append(self.Term.is_tpl(t))
    xs = []
    t = self.Term.tval(t)
    for i in range(0, n):
      xs.append(self.List.is_cons(t))
      t = self.List.tl(t)
    xs.append(t == self.List.nil)
    self.axs.append(Not(And(*xs)))
  
  # Not a Tuple
  def tuple_not_tpl_toZ3(self, term, num):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_tpl(t) == False)
  
  # Empty bitstring
  def empty_bitstr_toZ3(self, term):
    T, B = self.Term, self.BitStr
    t = self.term_toZ3(term)
    self.axs.extend([
      T.is_bin(t),
      B.is_bnil(T.bval(t)),
      T.bsz(t) == 0
    ])
  
  # Nonempty bitstring
  def nonempty_bitstr_toZ3(self, term1, term2, term):
    T, B = self.Term, self.BitStr
    s1 = term1["s"]
    s2 = term2["s"]
    t = self.term_toZ3(term)
    self.axs.extend([
      T.is_bin(t),
      B.is_bcons(T.bval(t)),
      T.bsz(t) > 0
    ])
    self.env.bind(s1, B.bhd(T.bval(t)))
    self.env.bind(s2, T.bin(T.bsz(t) - 1, B.btl(T.bval(t))))
  
  # Bitmach const true
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_const_true_toZ3(self, termRest, cnstValue, size, termBitstr):
    s = termRest["s"]
    t = self.bitmatch_const_false_toZ3_RV(cnstValue, size, termBitstr)
    self.env.bind(s, t)
  
  # Bitmach const false
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_const_false_toZ3(self, cnstValue, size, termBitstr):
    T, B = self.Term, self.BitStr
    axs = []
    cnst = self.term_toZ3(cnstValue)
    szTerm = self.term_toZ3(size)
    sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
    t = self.term_toZ3(termBitstr)
    axs.extend([T.is_bin(t), T.bsz(t) >= sz])
    t = T.bval(t)
    bits = []
    for _ in range(sz):
#      axs.append(B.is_bcons(t))
      bits.append(B.bhd(t))
      t = B.btl(t)
    concBits = Concat(*bits) if len(bits) > 1 else bits[0]
    concHelper = self.env.generate_bitvec(sz)
    axs.extend([T.is_int(cnst), T.ival(cnst) == BV2Int(concHelper), concHelper == concBits])
    self.axs.append(Not(And(*axs)))
  
  # Bitmatch var true
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_var_true_toZ3(self, term1, term2, size, termBitstr):
    T, B = self.Term, self.BitStr
    s1 = term1["s"]
    s2 = term2["s"]
    szTerm = self.term_toZ3(size)
    sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
    t = self.term_toZ3(termBitstr)
    origSz = T.bsz(t)
    if sz == 0:
      self.env.bind(s1, T.int(0)) # FIXME
      self.env.bind(s2, t)
    else:
      self.axs.extend([T.is_bin(t), origSz >= sz])
      t = T.bval(t)
      bits = []
      for _ in range(sz):
        self.axs.append(B.is_bcons(t))
        bits.append(B.bhd(t))
        t = B.btl(t)
      concBits = Concat(*bits) if len(bits) > 1 else bits[0]
      concHelper = self.env.generate_bitvec(sz)
      self.axs.extend([concHelper == concBits])
      self.env.bind(s1, T.int(BV2Int(concHelper)))
      self.env.bind(s2, T.bin(origSz - sz, t))
  
  # Bitmach var false
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_var_false_toZ3(self, size, termBitstr):
    T, B = self.Term, self.BitStr
    axs = []
    szTerm = self.term_toZ3(size)
    sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
    t = self.term_toZ3(termBitstr)
    axs.extend([T.is_bin(t), T.bsz(t) >= sz])
    self.axs.append(Not(And(*axs)))
  
  ### Reversed ###
  
  # NonEmpty List (Reversed)
  def list_nonempty_toZ3_RV(self, term):
    t = self.term_toZ3(term)
    xs = [
      self.Term.is_lst(t),
      self.List.is_cons(self.Term.lval(t)),
    ]
    self.axs.append(Not(And(*xs)))
  
  # Empty List (Reversed)
  def list_empty_toZ3_RV(self, term):
    t = self.term_toZ3(term)
    xs = [
      self.Term.is_lst(t),
      self.List.is_cons(self.Term.lval(t))
    ]
    self.axs.append(And(*xs))
  
  # Not a List (Reversed)
  def list_not_lst_toZ3_RV(self, term):
    t = self.term_toZ3(term)
    xs = [
      self.Term.is_lst(t),
      self.List.is_cons(self.Term.lval(t))
    ]
    self.axs.append(And(*xs))
  
  # Tuple of Size N (Reversed)
  def tuple_sz_toZ3_RV(self, term, num):
    t = self.term_toZ3(term)
    nTerm = self.term_toZ3(num)
    n = int(str(simplify(self.Term.ival(nTerm)))) # Expect num to represent an Integer
    xs = [self.Term.is_tpl(t)]
    t = self.Term.tval(t)
    for i in range(0, n):
      xs.append(self.List.is_cons(t))
      t = self.List.tl(t)
    xs.append(t == self.List.nil)
    self.axs.append(Not(And(*xs)))
  
  # Tuple of Not Size N (Reversed)
  def tuple_not_sz_toZ3_RV(self, term, num):
    t = self.term_toZ3(term)
    nTerm = self.term_toZ3(num)
    n = int(str(simplify(self.Term.ival(nTerm)))) # Expect num to represent an Integer
    self.axs.append(self.Term.is_tpl(t))
    t = self.Term.tval(t)
    for i in range(0, n):
      self.axs.append(self.List.is_cons(t))
      t = self.List.tl(t)
    self.axs.append(t == self.List.nil)
  
  # Not a Tuple (Reversed)
  def tuple_not_tpl_toZ3_RV(self, term, num):
    t = self.term_toZ3(term)
    nTerm = self.term_toZ3(num)
    n = int(str(simplify(self.Term.ival(nTerm)))) # Expect num to represent an Integer
    self.axs.append(self.Term.is_tpl(t))
    t = self.Term.tval(t)
    for i in range(0, n):
      self.axs.append(self.List.is_cons(t))
      t = self.List.tl(t)
    self.axs.append(t == self.List.nil)
  
  # Empty bitstring
  def empty_bitstr_toZ3_RV(self, term):
    T, B = self.Term, self.BitStr
    t = self.term_toZ3(term)
    self.axs.extend([
      T.is_bin(t),
      T.bsz(t) > 0,
      B.is_bcons(T.bval(t))
    ])
  
  # Nonempty bitstring
  def nonempty_bitstr_toZ3_RV(self, term1, term2, term):
    T, B = self.Term, self.BitStr
    t = self.term_toZ3(term)
    self.axs.extend([
      T.is_bin(t),
      T.bsz(t) == 0,
      B.is_bnil(T.bval(t))
    ])
  
  # Bitmach const true (reversed)
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_const_true_toZ3_RV(self, termRest, cnstValue, size, termBitstr):
    T, B = self.Term, self.BitStr
    axs = []
    cnst = self.term_toZ3(cnstValue)
    szTerm = self.term_toZ3(size)
    sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
    t = self.term_toZ3(termBitstr)
    axs.extend([T.is_bin(t), T.bsz(t) >= sz])
    t = T.bval(t)
    bits = []
    for _ in range(sz):
#      axs.append(B.is_bcons(t))
      bits.append(B.bhd(t))
      t = B.btl(t)
    concBits = Concat(*bits) if len(bits) > 1 else bits[0]
    concHelper = self.env.generate_bitvec(sz)
    axs.extend([T.is_int(cnst), T.ival(cnst) == BV2Int(concHelper), concHelper == concBits])
    self.axs.append(Not(And(*axs)))
#    axs.extend([T.is_int(cnst), concHelper == concBits])
#    self.axs.append(And(*axs))
#    self.axs.append(T.ival(cnst) != BV2Int(concHelper))
  
  # Bitmach const false (reversed)
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_const_false_toZ3_RV(self, cnstValue, size, termBitstr):
    T, B = self.Term, self.BitStr
    cnst = self.term_toZ3(cnstValue)
    szTerm = self.term_toZ3(size)
    sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
    if sz == 0:
      return T.bin(0, B.bnil)
    else:
      t = self.term_toZ3(termBitstr)
      origSz = simplify(T.bsz(t))
      self.axs.extend([simplify(T.is_bin(t)), origSz >= sz])
      t = T.bval(t)
      bits = []
      for _ in range(sz):
        self.axs.append(B.is_bcons(t))
        bits.append(simplify(B.bhd(t)))
        t = B.btl(t)
      concBits = Concat(*bits) if len(bits) > 1 else bits[0]
      concHelper = self.env.generate_bitvec(sz)
      self.axs.extend([simplify(T.is_int(cnst)), T.ival(cnst) == BV2Int(concHelper), concHelper == concBits])
      return T.bin(origSz - sz, t)
  
  # Bitmatch var true (reversed)
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_var_true_toZ3_RV(self, term1, term2, size, termBitStr):
    self.bitmatch_var_false_toZ3(size, termBitStr)
  
  # Bitmach var false (reversed)
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def bitmatch_var_false_toZ3_RV(self, size, termBitstr):
    T, B = self.Term, self.BitStr
    axs = []
    szTerm = self.term_toZ3(size)
    sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
    t = self.term_toZ3(termBitstr)
    axs.extend([T.is_bin(t), T.bsz(t) >= sz])
    self.axs.append(And(*axs))
  
  # ----------------------------------------------------------------------
  # Define Type Specs
  # ----------------------------------------------------------------------
  
  def spec_toZ3(self, *spec):
    axms = []
    for cl in spec:
      xs = self.spec_clause_toZ3(cl)
      axms.append(xs)
    ax = Or(*axms)
    self.axs.append(ax)
  
  def spec_clause_toZ3(self, cl):
    pms = self.env.params
    axms = []
    for pm, tp in zip(pms, cl["p"]):
      s = self.env.lookup(pm)
      xs = self.typedef_toZ3(s, tp)
      if xs != None:
        axms.append(xs)
    return And(*axms)
  
  def typedef_toZ3(self, s, tp):
    opts = {
      cc.JSON_ERLTYPE_ANY: self.type_any_toZ3,
      cc.JSON_ERLTYPE_ATOM: self.type_atom_toZ3,
      cc.JSON_ERLTYPE_ATOMLIT: self.type_atomlit_toZ3,
      cc.JSON_ERLTYPE_FLOAT: self.type_float_toZ3,
      cc.JSON_ERLTYPE_INTEGER: self.type_integer_toZ3,
      cc.JSON_ERLTYPE_INTEGERLIT: self.type_integerlit_toZ3,
      cc.JSON_ERLTYPE_LIST: self.type_list_toZ3,
      cc.JSON_ERLTYPE_NIL: self.type_nil_toZ3,
      cc.JSON_ERLTYPE_TUPLE: self.type_tuple_toZ3,
      cc.JSON_ERLTYPE_TUPLEDET: self.type_tupledet_toZ3,
      cc.JSON_ERLTYPE_UNION: self.type_union_toZ3,
      cc.JSON_ERLTYPE_RANGE: self.type_range_toZ3,
      cc.JSON_ERLTYPE_NONEMPTY_LIST: self.type_nonempty_list_toZ3,
      cc.JSON_ERLTYPE_BITSTRING: self.type_bitstring_toZ3
    }
    tpcode = tp["tp"]
    arg = tp["a"] if "a" in tp else None
    return opts[tpcode](s, arg)
  
  def type_any_toZ3(self, s, arg):
    return None
  
  def type_atom_toZ3(self, s, arg):
    return self.Term.is_atm(s)
  
  def type_atomlit_toZ3(self, s, arg):
    atm = self.term_toZ3(arg)
    return (s == atm)
  
  def type_bitstring_toZ3(self, s, arg):
    T = self.Term
    sz = arg['v']  # Assume it's an integer
    if sz == 1: # TODO Do sth with binaries
      return T.is_bin(s)
    else:
      return And(T.is_bin(s), simplify(T.bsz(s)) % sz == 0)
  
  def type_float_toZ3(self, s, arg):
    return self.Term.is_real(s)
  
  def type_integer_toZ3(self, s, arg):
    return self.Term.is_int(s)
  
  def type_integerlit_toZ3(self, s, arg):
    i = self.term_toZ3(arg)
    return (s == i)
  
  def type_range_toZ3(self, s, limits):
    T = self.Term
    axs = [T.is_int(s)]
    if not ("tp" in limits[0] and limits[0]["tp"] == cc.JSON_ERLTYPE_INTEGER):
      l1 = self.term_toZ3(limits[0])
      axs.append(T.ival(s) >= T.ival(l1))
    if not ("tp" in limits[1] and limits[1]["tp"] == cc.JSON_ERLTYPE_INTEGER):
      l2 = self.term_toZ3(limits[1])
      axs.append(T.ival(s) <= T.ival(l2))
    return And(*axs)
  
  def type_nonempty_list_toZ3(self, s, tp):
    T, L = self.Term, self.List
    ax = self.type_list_toZ3(s, tp)
    return And(ax, s != T.lst(L.nil))
  
  def type_list_toZ3(self, s, tp):
    if cglb.__LISTS_INTERP__ == cglb.LISTS_EXPAND:
      return self.type_list_expand_toZ3(s, tp)
    else:
      return self.type_list_forall_toZ3(s, tp)
  
  def type_list_expand_toZ3(self, s, tp):
    T, L = self.Term, self.List
    x = T.lval(s)
    axs = []
    for _ in range(10):
      ax_hd = self.typedef_toZ3(L.hd(x), tp)
      # Override for lists of type [any()]
      if ax_hd == None:
        return T.is_lst(s)
      axs.append( (L.is_nil(x), L.is_nil(x), ax_hd) )
      x = L.tl(x)
    
    ax_if = True
    for (cond, ax_true, ax_false) in reversed(axs):
      ax_if = If(cond, ax_true, And(ax_false, ax_if))
    return And(T.is_lst(s), ax_if)
  
  
  def type_list_forall_toZ3(self, s, tp):
    T, L = self.Term, self.List
    f = self.env.generate_func(T, BoolSort())
    x = self.env.generate_const(T)
    
    ax_nil = [
      T.is_lst(x),
      L.is_nil(T.lval(x)),
      f(x) == True
    ]
    
    ax_cons = [
      T.is_lst(x),
      L.is_cons(T.lval(x)),
    ]
    ax_hd = self.typedef_toZ3(L.hd(T.lval(x)), tp)
    if ax_hd != None:
      ax_cons.append(ax_hd)
    # Override for lists of type [any()]
    else:
      return T.is_lst(s)
    ax_cons.extend([
      f(T.lst(L.tl(T.lval(x)))) == True,
      f(x) == True
    ])
    
    if cglb.__LISTS_INTERP__ == cglb.LISTS_FORALL_PATS:
      ax_forall = ForAll(x,
        Or(
          And(*ax_nil),
          And(*ax_cons),
          f(x) == False
        ),
        patterns=[f(x)]
      )
    else:
      ax_forall = ForAll(x,
        Or(
          And(*ax_nil),
          And(*ax_cons),
          f(x) == False
        )
      )
    
    self.quantifier_axs.append(ax_forall)
    return f(s) == True
  
  
  def type_nil_toZ3(self, s, arg):
    T, L = self.Term, self.List
    return And(
      T.is_lst(s),
      L.is_nil(T.lval(s))
    )
  
  def type_tuple_toZ3(self, s, arg):
    return self.Term.is_tpl(s)
  
  def type_tupledet_toZ3(self, s, types):
    T, L = self.Term, self.List
    axms = [T.is_tpl(s)]
    t = T.tval(s)
    for tp in types:
      axms.append(L.is_cons(t))
      h, t = L.hd(t), L.tl(t)
      ax = self.typedef_toZ3(h, tp)
      if ax != None:
        axms.append(ax)
    axms.append(L.is_nil(t))
    return And(*axms)
  
  def type_union_toZ3(self, s, types):
    axms = []
    for tp in types:
      ax = self.typedef_toZ3(s, tp)
      if ax != None:
        axms.append(ax)
    return Or(*axms)
  
  # ----------------------------------------------------------------------
  # Other Important Commands
  # ----------------------------------------------------------------------
  
  # Entry Point MFA's symbolic parameters
  def params_toZ3(self, *args):
    e = self.env
    pms = []
    for x in args:
      s = x["s"]
      p = e.freshVar(s, self.Term)
      pms.append(p)
      e.addParam(s)
    return pms
  
  # Unfold a symbolic tuple
  def unfold_tuple_toZ3(self, *terms):
    t, ts = self.term_toZ3(terms[0]), terms[1:]
    self.axs.append(self.Term.is_tpl(t))
    t = self.Term.tval(t)
    for x in ts:
      s = x["s"]
      self.axs.append(self.List.is_cons(t))
      self.env.bind(s, self.List.hd(t))
      t = self.List.tl(t)
    self.axs.append(t == self.List.nil)
  
  # Unfold a symbolic list
  def unfold_list_toZ3(self, *terms):
    t, ts = self.term_toZ3(terms[0]), terms[1:]
    self.axs.append(self.Term.is_lst(t))
    t = self.Term.lval(t)
    for x in ts:
      s = x["s"]
      self.axs.append(self.List.is_cons(t))
      self.env.bind(s, self.List.hd(t))
      t = self.List.tl(t)
    self.axs.append(t == self.List.nil)
  
  # Encode a value to a bitstring.
  # TODO For now, expects size to be a concrete integer and encodedValue to be an integer.
  def make_bitstr_toZ3(self, symb, encodedValue, size):
    T, B = self.Term, self.BitStr
    s = symb["s"]
    enc = self.term_toZ3(encodedValue)
    szTerm = self.term_toZ3(size)
    sz = int(str(simplify(T.ival(szTerm)))) # Expect size to represent an Integer
    if sz == 0:
      self.env.bind(s, T.bin(0, B.bnil))
    else:
      # Add axioms.
      bits = [self.env.generate_bitvec(1) for _ in range(sz)]
      concBits = Concat(*bits) if len(bits) > 1 else bits[0]
      concHelper = self.env.generate_bitvec(sz)
      self.axs.extend([T.is_int(enc), T.ival(enc) == BV2Int(concHelper), concHelper == concBits])
      # Bind the symbolic result.
      b = B.bnil
      for bit in reversed(bits):
        b = B.bcons(bit, b)
      self.env.bind(s, T.bin(sz, b))
  
  def concat_segs_toZ3(self, *terms):
    T, B = self.Term, self.BitStr
    term1, term2, ts = terms[0], terms[1], terms[2:]
    s = term1["s"]
    t = self.term_toZ3(term2)
    sz = T.bsz(t)
    self.axs.append(T.is_bin(t))
    t = T.bval(t)
    for x in reversed(ts):
      if "s" in x:
        t = B.bcons(self.term_toZ3(x), t)
      else:
        nTerm = self.term_toZ3(x)
        n = int(str(simplify(T.ival(nTerm))))
        t = B.bcons(BitVecVal(n, 1), t)
    self.env.bind(s, T.bin(sz + len(ts), t))
  
  # ----------------------------------------------------------------------
  # Operations on tuples
  # ----------------------------------------------------------------------
  
  def tcons_toZ3(self, *terms):
    T, L = self.Term, self.List
    s, ts = terms[0]["s"], map(self.term_toZ3, terms[1:])
    t = L.nil
    for x in reversed(ts):
      t = L.cons(x, t)
    self.env.bind(s, T.tpl(t))
  
  # ----------------------------------------------------------------------
  # Operations on lists
  # ----------------------------------------------------------------------
  
  ### Get the head of a list ###
  
  def hd_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      self.Term.is_lst(t2),
      self.List.is_cons(self.Term.lval(t2))
    ])
    self.env.bind(s, self.List.hd(self.Term.lval(t2)))
  
  # (Reversed)
  def hd_toZ3_RV(self, term1, term2):
    t2 = self.term_toZ3(term2)
    self.axs.append(Or(
      self.Term.is_lst(t2) == False,
      And(self.Term.is_lst(t2), self.List.is_nil(self.Term.lval(t2)))
    ))
  
  ### Get the tail of a list ###
  
  def tl_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      self.Term.is_lst(t2),
      self.List.is_cons(self.Term.lval(t2))
    ])
    self.env.bind(s, self.Term.lst(self.List.tl(self.Term.lval(t2))))
  
  # (Reversed)
  def tl_toZ3_RV(self, term1, term2):
    t2 = self.term_toZ3(term2)
    self.axs.append(Or(
      self.Term.is_lst(t2) == False,
      And(self.Term.is_lst(t2), self.List.is_nil(self.Term.lval(t2)))
    ))
  
  ### Simulate the cons operation ###
  
  def cons_toZ3(self, term, term1, term2):
    T = self.Term
    L = self.List
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(T.is_lst(t2))
    self.env.bind(s, T.lst(L.cons(t1, T.lval(t2))))
  
  # ----------------------------------------------------------------------
  # Query types
  # ----------------------------------------------------------------------
  
  ### Is a term an integer ###
  
  def is_integer_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_int(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Is a term an atom ###
  
  def is_atom_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_atm(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Is a term a float ###
  
  def is_float_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_real(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Is a term a list ###
  
  def is_list_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_lst(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Is a term a tuple ###
  
  def is_tuple_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_tpl(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Is a term a boolean ###
  
  def is_boolean_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      Or(t2 == self.atmTrue, t2 == self.atmFalse),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Is a term a number ###
  
  def is_number_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      Or(self.Term.is_real(t2), self.Term.is_int(t2)),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Is a term a bitstring ###
  
  def is_bitstring_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_bin(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  # ----------------------------------------------------------------------
  # Arithmetic operations
  # ----------------------------------------------------------------------
  
  ## Add two numbers ###
  
  def plus_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      Or(T.is_int(t1), T.is_real(t1)),
      Or(T.is_int(t2), T.is_real(t2))
    ])
    self.env.bind(s, If(
      And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
      T.int(T.ival(t1) + T.ival(t2)),         # t = int(t1+t2)
      If(
        And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
        T.real(T.rval(t1) + T.rval(t2)),      # t = real(t1+t2)
        If(
          And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
          T.real(T.ival(t1) + T.rval(t2)),    # t = real(t1+t2)
          T.real(T.rval(t1) + T.ival(t2))     # t1: Real, t2: Int, t = real(t1+t2)
        )
      )
    ))
  
  ## Subtract two numbers ###
  
  def minus_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      Or(T.is_int(t1), T.is_real(t1)),
      Or(T.is_int(t2), T.is_real(t2))
    ])
    self.env.bind(s, If(
      And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
      T.int(T.ival(t1) - T.ival(t2)),         # t = int(t1-t2)
      If(
        And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
        T.real(T.rval(t1) - T.rval(t2)),      # t = real(t1-t2)
        If(
          And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
          T.real(T.ival(t1) - T.rval(t2)),    # t = real(t1-t2)
          T.real(T.rval(t1) - T.ival(t2))     # t1: Real, t2: Int, t = real(t1-t2)
        )
      )
    ))
  
  ## Multiply two numbers ###
  
  def times_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      Or(T.is_int(t1), T.is_real(t1)),
      Or(T.is_int(t2), T.is_real(t2))
    ])
    self.env.bind(s, If(
      And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
      T.int(T.ival(t1) * T.ival(t2)),         # t = int(t1*t2)
      If(
        And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
        T.real(T.rval(t1) * T.rval(t2)),      # t = real(t1*t2)
        If(
          And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
          T.real(T.ival(t1) * T.rval(t2)),    # t = real(t1*t2)
          T.real(T.rval(t1) * T.ival(t2))     # t1: Real, t2: Int, t = real(t1*t2)
        )
      )
    ))
  
  ## Divide two numbers ###
  
  def rdiv_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      Or(T.is_int(t1), T.is_real(t1)),
      Or(
        And(T.is_int(t2), T.ival(t2) != 0),
        And(T.is_real(t2), T.rval(t2) != 0.0)
      ),
    ])
    self.env.bind(s, If(
      And(T.is_int(t1), T.is_int(t2)),          # t1, t2: Int
      T.real(T.ival(t1) / ToReal(T.ival(t2))),  # t = int(t1/t2)
      If(
        And(T.is_real(t1), T.is_real(t2)),      # t1, t2: Real
        T.real(T.rval(t1) / T.rval(t2)),        # t = real(t1/t2)
        If(
          And(T.is_int(t1), T.is_real(t2)),     # t1: Int, t2: Real
          T.real(T.ival(t1) / T.rval(t2)),      # t = real(t1/t2)
          T.real(T.rval(t1) / T.ival(t2))       # t1: Real, t2: Int, t = real(t1/t2)
        )
      )
    ))
  
  ### Integer division with natural numbers
  
  def idiv_nat_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      T.is_int(t1),
      T.is_int(t2),
      T.ival(t1) >= 0,
      T.ival(t2) > 0
    ])
    self.env.bind(s, T.int(T.ival(t1) / T.ival(t2)))
  
  ### Remainder of integer division with natural numbers
  
  def rem_nat_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      T.is_int(t1),
      T.is_int(t2),
      T.ival(t1) >= 0,
      T.ival(t2) > 0
    ])
    self.env.bind(s, T.int(T.ival(t1) % T.ival(t2)))
  
  ### Unary operation
  
  def unary_toZ3(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(Or(
      T.is_int(t1), T.is_real(t1)
    ))
    self.env.bind(s, If(
      T.is_int(t1),
      T.int( - T.ival(t1) ),
      T.real( - T.rval(t1) )
    ))
  
  ### Exponential
  
  ## FIXME Z3 only support nonlinear polynomial equations. Should look at http://www.cl.cam.ac.uk/~lp15/papers/Arith/.
  def pow_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      Or(T.is_int(t1), T.is_real(t1)),
      Or(T.is_int(t2), T.is_real(t2))
    ])
    self.env.bind(s, If(
      And(T.is_int(t1), T.is_int(t2)),        # t1, t2: Int
      T.real(T.ival(t1) ** T.ival(t2)),       # t = real(t1^t2)
      If(
        And(T.is_real(t1), T.is_real(t2)),    # t1, t2: Real
        T.real(T.rval(t1) ** T.rval(t2)),     # t = real(t1^t2)
        If(
          And(T.is_int(t1), T.is_real(t2)),   # t1: Int, t2: Real
          T.real(T.ival(t1) ** T.rval(t2)),   # t = real(t1^t2)
          T.real(T.rval(t1) ** T.ival(t2))    # t1: Real, t2: Int, t = real(t1^t2)
        )
      )
    ))
  
  # ----------------------------------------------------------------------
  # Comparisons
  # ----------------------------------------------------------------------
  
  ### Equality of two terms
  
  def equal_toZ3(self, term, term1, term2):
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      t1 == t2,
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Inequality of two terms
  
  def unequal_toZ3(self, term, term1, term2):
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      t1 == t2,
      self.atmFalse,
      self.atmTrue
    ))
  
  ### Compare two integers (<)
  
  def lt_integers_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      T.is_int(t1), T.is_int(t2)
    ])
    self.env.bind(s, If(
      T.ival(t1) < T.ival(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### Compare two floats (<)
  
  def lt_floats_toZ3(self, term, term1, term2):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      T.is_real(t1), T.is_real(t2)
    ])
    self.env.bind(s, If(
      T.rval(t1) < T.rval(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  # ----------------------------------------------------------------------
  # Type conversions
  # ----------------------------------------------------------------------
  
  ### Convert a number to float
  
  def float_toZ3(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(Or(
      T.is_int(t1),
      T.is_real(t1)
    ))
    self.env.bind(s, If(
      T.is_real(t1),
      t1,
      T.real( ToReal(T.ival(t1)) )
    ))
  
  ## Convert a list to a tuple
  
  def list_to_tuple_toZ3(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(T.is_lst(t1))
    self.env.bind(s, T.tpl(T.lval(t1)))
  
  ## Convert a tuple to a list
  
  def tuple_to_list_toZ3(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(T.is_tpl(t1))
    self.env.bind(s, T.lst(T.tval(t1)))
  
  # ----------------------------------------------------------------------
  # Bogus operations
  # 
  # They are used for their side-effects in Erlang and are transparent
  # to Z3.
  # ----------------------------------------------------------------------
  
  ### Identity function ###
  
  def bogus_toZ3(self, term, term1):
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.env.bind(s, t1)
  
  # ----------------------------------------------------------------------
  # Operations on atoms
  # ----------------------------------------------------------------------
  
  ### Is an empty atom
  
  def atom_nil_toZ3(self, term, term1):
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.env.bind(s, If(
      t1 == self.Term.atm(self.Atom.anil),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### The first character in an atom
  
  def atom_head_toZ3(self, term, term1):
    T = self.Term
    A = self.Atom
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.extend([
      T.is_atm(t1),
      A.is_acons(T.aval(t1))
    ])
    self.env.bind(s,
      T.int(A.ahd(T.aval(t1)))
    )
  
  ### The atom except its first character
  
  def atom_tail_toZ3(self, term, term1):
    T = self.Term
    A = self.Atom
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.extend([
      T.is_atm(t1),
      A.is_acons(T.aval(t1))
    ])
    self.env.bind(s,
      T.atm(A.atl(T.aval(t1)))
    )

###############################################################################
# Unit Tests
###############################################################################

def test_decoder_simple():
  erlz3 = ErlangZ3()
  T, L, A, B = erlz3.Term, erlz3.List, erlz3.Atom, erlz3.BitStr
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
  decode_and_check(erlz3, terms)

def test_decoder_complex():
  erlz3 = ErlangZ3()
  T, L = erlz3.Term, erlz3.List
  s1, s2 = "0.0.0.39316", "0.0.0.39317"
  erlz3.env.bind(s1, T.int(1))
  erlz3.env.bind(s2, T.int(2))
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
  decode_and_check(erlz3, terms)

def decode_and_check(erlz3, terms):
  for x, y in terms:
    z = TermDecoder(erlz3).toZ3(x, x["d"] if "d" in x else {})
    s = Solver()
    s.add(z == y)
    assert s.check() == sat, "Decoded {} is not {} but {}".format(x, y, z)

def test_encoder():
  erlz3 = ErlangZ3()
  T, L, A, B = erlz3.Term, erlz3.List, erlz3.Atom, erlz3.BitStr
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
    ),
    
  ]
  for x, y in terms:
    z = TermEncoder(erlz3).fromZ3(x)
    assert z == y, "Encoded {} is not {} but {}".format(x, y, z)

def test_model():
  erlz3 = ErlangZ3()
  T, L, A, B = erlz3.Term, erlz3.List, erlz3.Atom, erlz3.BitStr
  s1, s2, s3, s4 = "0.0.0.39316", "0.0.0.39317", "0.0.0.39318", "0.0.0.39319"
  expected = {
    s1: {"t":cc.JSON_TYPE_INT,"v":42},
    s2: {"t":cc.JSON_TYPE_ATOM,"v":[111,107]},
    s3: {"t":cc.JSON_TYPE_LIST,"v":[{"t":cc.JSON_TYPE_BITSTRING,"v":[]},{"t":cc.JSON_TYPE_INT,"v":2}]},
    s4: {"t":cc.JSON_TYPE_ANY}
  }
  [p1, p2, p3, p4] = erlz3.params_toZ3({"s":s1}, {"s":s2}, {"s":s3}, {"s":s4})
  erlz3.axs.extend([
    p1 == T.int(42),
    p2 == T.atm(A.acons(111,A.acons(107,A.anil))),
    p3 == T.lst(L.cons(T.bin(0, B.bnil),L.cons(T.int(2),L.nil)))
  ])
  erlz3.add_axioms()
  assert erlz3.solve() == cc.SOLVER_STATUS_SAT, "Model in unsatisfiable"
  model = erlz3.encode_model()
  assert model != [], "The model is empty"
  for smb, v in model:
    s = smb["s"]
    assert expected[s] == v, "{} is {} instead of {}".format(s, expected[s], v)

def test_commands():
  ss = [{"s":"0.0.0.{0:05d}".format(i)} for i in range(50)]
  anyTerm = {"t":cc.JSON_TYPE_ANY}
  trueTerm = {"t":cc.JSON_TYPE_ATOM,"v":[116,114,117,101]}
  falseTerm = {"t":cc.JSON_TYPE_ATOM,"v":[102,97,108,115,101]}
  cmds = [
    ## Defining ss[0]
    ({"c":cc.OP_IS_INTEGER, "a":[ss[2], ss[0]]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE,"a":[deepcopy(trueTerm), ss[2]]}, False),
    ({"c":cc.OP_PLUS, "a":[ss[3], ss[0], {"t":cc.JSON_TYPE_INT,"v":1}]}, False),
    ({"c":cc.OP_EQUAL, "a":[ss[4], ss[3], {"t":cc.JSON_TYPE_INT,"v":3}]}, False),
    ({"c":cc.OP_GUARD_TRUE, "a":[ss[4]]}, False),
    ({"c":cc.OP_GUARD_TRUE, "a":[deepcopy(falseTerm)]}, True),
    ## Defining ss[1]
    ({"c":cc.OP_TUPLE_NOT_TPL, "a":[ss[1], {"t":cc.JSON_TYPE_INT,"v":1}]}, False),
    ({"c":cc.OP_TUPLE_SZ, "a":[ss[1], {"t":cc.JSON_TYPE_INT,"v":1}]}, True),
    ({"c":cc.OP_IS_FLOAT, "a":[ss[5], ss[1]]}, False),
    ({"c":cc.OP_GUARD_FALSE, "a":[ss[5]]}, True),
    ({"c":cc.OP_RDIV, "a":[ss[6], ss[1], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
    ({"c":cc.OP_UNARY, "a":[ss[7], ss[6]]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[7], {"t":cc.JSON_TYPE_FLOAT,"v":-10.25}]}, False),
    ## Defining ss[8]
    ({"c":cc.OP_IS_ATOM, "a":[ss[9], ss[8]]}, False),
    ({"c":cc.OP_ATOM_NIL, "a":[ss[10], ss[8]]}, False),
    ({"c":cc.OP_GUARD_FALSE, "a":[ss[10]]}, False),
    ({"c":cc.OP_ATOM_HEAD, "a":[ss[11], ss[8]]}, False),
    ({"c":cc.OP_ATOM_TAIL, "a":[ss[12], ss[8]]}, False),
    ({"c":cc.OP_MINUS, "a":[ss[13], ss[11], {"t":cc.JSON_TYPE_INT,"v":10}]}, False),
    ({"c":cc.OP_UNEQUAL, "a":[ss[14], ss[13], {"t":cc.JSON_TYPE_INT,"v":101}]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[14], deepcopy(trueTerm)]}, True),
    ({"c":cc.OP_MATCH_EQUAL_FALSE, "a":[ss[12], {"t":cc.JSON_TYPE_ATOM,"v":[107]}]}, True),
    ## Defining ss[15]
    ({"c":cc.OP_IS_LIST, "a":[ss[16], ss[15]]}, False),
    ({"c":cc.OP_GUARD_TRUE, "a":[ss[16]]}, False),
    ({"c":cc.OP_LIST_NON_EMPTY, "a":[ss[15]]}, False),
    ({"c":cc.OP_LIST_EMPTY, "a":[ss[15]]}, True),
    ({"c":cc.OP_LIST_NOT_LST, "a":[ss[15]]}, True),
    ({"c":cc.OP_HD, "a":[ss[17], ss[15]]}, False),
    ({"c":cc.OP_TL, "a":[ss[18], ss[15]]}, False),
    ({"c":cc.OP_LIST_EMPTY, "a":[ss[18]]}, False),
    ({"c":cc.OP_LIST_NON_EMPTY, "a":[ss[18]]}, True),
    ({"c":cc.OP_IS_BOOLEAN, "a":[ss[19], ss[17]]}, False),
    ({"c":cc.OP_MATCH_EQUAL_FALSE, "a":[ss[19], deepcopy(falseTerm)]}, False),
    ({"c":cc.OP_GUARD_TRUE, "a":[ss[17]]}, False),
    ({"c":cc.OP_LIST_NOT_LST, "a":[ss[17]]}, False),
    ({"c":cc.OP_TUPLE_SZ, "a":[ss[17], {"t":cc.JSON_TYPE_INT,"v":2}]}, True),
    ## Defining ss[20]
    ({"c":cc.OP_IS_TUPLE, "a":[ss[21], ss[20]]}, False),
    ({"c":cc.OP_GUARD_TRUE, "a":[ss[21]]}, False),
    ({"c":cc.OP_TUPLE_SZ, "a":[ss[20], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
    ({"c":cc.OP_TUPLE_NOT_SZ, "a":[ss[20], {"t":cc.JSON_TYPE_INT,"v":2}]}, True),
    ({"c":cc.OP_UNFOLD_TUPLE, "a":[ss[20], ss[22], ss[23]]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[22], {"t":cc.JSON_TYPE_INT,"v":42}]}, False),
    ({"c":cc.OP_TIMES, "a":[ss[24], ss[23], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[24], {"t":cc.JSON_TYPE_INT,"v":10}]}, False),
    ({"c":cc.OP_IS_NUMBER, "a":[ss[25], ss[24]]}, False),
    ({"c":cc.OP_GUARD_TRUE, "a":[ss[25]]}, False),
    ({"c":cc.OP_TUPLE_NOT_TPL, "a":[ss[20], {"t":cc.JSON_TYPE_INT,"v":2}]}, True),
    ({"c":cc.OP_FLOAT, "a":[ss[26], ss[22]]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[26], {"t":cc.JSON_TYPE_FLOAT,"v":42.0}]}, False),
    ({"c":cc.OP_BOGUS, "a":[ss[26], ss[26]]}, False),
    ## Defining ss[27]
    ({"c":cc.OP_IDIV_NAT, "a":[ss[28], {"t":cc.JSON_TYPE_INT,"v":5}, {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
    ({"c":cc.OP_REM_NAT, "a":[ss[29], {"t":cc.JSON_TYPE_INT,"v":5}, {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
    ({"c":cc.OP_PLUS, "a":[ss[30], ss[28], ss[29]]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[30], {"t":cc.JSON_TYPE_INT,"v":3}]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[27], ss[30]]}, False),
    ({"c":cc.OP_LT_INT, "a":[ss[31], {"t":cc.JSON_TYPE_INT,"v":2}, ss[27]]}, False),
    ({"c":cc.OP_GUARD_TRUE, "a":[ss[31]]}, False),
    ({"c":cc.OP_LT_FLOAT, "a":[ss[32], {"t":cc.JSON_TYPE_FLOAT,"v":3.14}, {"t":cc.JSON_TYPE_FLOAT,"v":1.42}]}, False),
    ({"c":cc.OP_GUARD_FALSE, "a":[ss[32]]}, False),
    ## Defining ss[33]
    ({"c":cc.OP_UNFOLD_LIST, "a":[ss[33], ss[34], ss[35]]}, False),
    ({"c":cc.OP_TCONS, "a":[ss[36], ss[34], ss[35]]}, False),
    ({"c":cc.OP_LIST_TO_TUPLE, "a":[ss[36], ss[33]]}, False),
    ({"c":cc.OP_CONS, "a":[ss[37], ss[34], ss[35]]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[35], {"t":cc.JSON_TYPE_LIST,"v":[]}]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[34], {"t":cc.JSON_TYPE_INT,"v":2}]}, False),
    ## Defining ss[38]
    ({"c":cc.OP_MAKE_BITSTR, "a":[ss[39], ss[38], {"t":cc.JSON_TYPE_INT,"v":3}]}, False),
    ({"c":cc.OP_MATCH_EQUAL_TRUE, "a":[ss[39], {"t":cc.JSON_TYPE_BITSTRING,"v":[1,0,1]}]}, False),
  ]
  expected = {
    ss[0]["s"]: {"t":cc.JSON_TYPE_INT,"v":2},
    ss[1]["s"]: {"t":cc.JSON_TYPE_FLOAT,"v":20.5},
    ss[8]["s"]: {"t":cc.JSON_TYPE_ATOM,"v":[111,107]},
    ss[15]["s"]: {"t":cc.JSON_TYPE_LIST,"v":[deepcopy(trueTerm)]},
    ss[20]["s"]: {"t":cc.JSON_TYPE_TUPLE,"v":[{"t":cc.JSON_TYPE_INT,"v":42}, {"t":cc.JSON_TYPE_INT,"v":5}]},
    ss[27]["s"]: {"t":cc.JSON_TYPE_INT,"v":3},
    ss[33]["s"]: {"t":cc.JSON_TYPE_LIST,"v":[{"t":cc.JSON_TYPE_INT,"v":2}, {"t":cc.JSON_TYPE_LIST,"v":[]}]},
    ss[38]["s"]: {"t":cc.JSON_TYPE_INT,"v":5},
  }
  erlz3 = ErlangZ3()
  erlz3.params_toZ3(ss[0], ss[1], ss[8], ss[15], ss[20], ss[27], ss[33], ss[38])
  for cmd, rvs in cmds:
    erlz3.command_toZ3(cmd["c"], cmd, rvs)
  erlz3.add_axioms()
  assert erlz3.solve() == cc.SOLVER_STATUS_SAT, "Model in unsatisfiable"
  model = erlz3.encode_model()
  assert model != [], "The model is empty"
  for smb, v in model:
    assert v == expected[smb["s"]], "{} is {} instead of {}".format(smb["s"], v, expected[smb["s"]])

if __name__ == '__main__':
  import json
  from copy import deepcopy
  cglb.init()
  test_decoder_simple()
  test_decoder_complex()
  test_encoder()
  test_model()
  test_commands()
