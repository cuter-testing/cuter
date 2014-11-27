#!/usr/bin/env python
# -*- coding: utf-8 -*-

import json
from z3 import *
import cuter_global as cglb
import cuter_common as cc

class Env:
  def __init__(self):
    self.cnt = 0
    self.e = {}
    self.params = []
  
  def add_param(self, x):
    self.params.append(x)
  
  def bind(self, s, v):
    self.e[s] = v
  
  def lookup(self, x):
    e = self.e
    return None if x not in e else e[x]
  
  def fresh_var(self, s, Type):
    self.cnt += 1
    x =  Const("x%s" % self.cnt, Type)
    self.e[s] = x
    return x

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

class TermDecoder:
  def __init__(self, eZ3):
    self.eZ3 = eZ3
    self.env = []
  
  def toZ3(self, t, dct):
    if "s" in t:
      x = self.eZ3.env.lookup(t["s"])
      assert x is not None, "Symbolic Variable lookup"
      return x
    if "l" in t:
      if "l" in self.env:
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



class ErlangZ3:
  def __init__(self):
    self.Term, self.List, self.Atom = self.erlang_type_system()
    self.env = Env()
    self.axs = []
    self.slv = Solver()
    
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
    # Term
    Term.declare('int', ('ival', IntSort()))
    Term.declare('real', ('rval', RealSort()))
    Term.declare('lst', ('lval', List))
    Term.declare('tpl', ('tval', List))
    Term.declare('atm', ('aval', Atom))
    # List
    List.declare('nil')
    List.declare('cons', ('hd', Term), ('tl', List))
    # Atom
    Atom.declare('anil')
    Atom.declare('acons', ('ahd', IntSort()), ('atl', Atom))
    # Return Datatypes
    return CreateDatatypes(Term, List, Atom)
  
  # Reset the solver
  def reset_solver(self):
    self.slv = Solver()
  
  # Add the axioms to the solver
  def add_axioms(self):
    self.slv.add(self.axs)
  
  # Solve a Constraint Set
  def solve(self):
    self.check = self.slv.check()
    if (self.check == sat):
      self.model = self.slv.model()
      return True
    else:
      return False
  
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
      # Other important commands
      cc.OP_PARAMS: self.params_toZ3,
      cc.OP_UNFOLD_TUPLE: self.unfold_tuple_toZ3,
      cc.OP_UNFOLD_LIST: self.unfold_list_toZ3,
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
      # Erlang BIFs
      cc.OP_HD: self.hd_toZ3_RV,
      cc.OP_TL: self.tl_toZ3_RV,
      cc.OP_PLUS: self.plus_toZ3_RV,
      cc.OP_MINUS: self.minus_toZ3_RV,
      cc.OP_TIMES: self.times_toZ3_RV,
      cc.OP_RDIV: self.rdiv_toZ3_RV,
      cc.OP_IDIV_NAT: self.idiv_nat_toZ3_RV,
      cc.OP_REM_NAT: self.rem_nat_toZ3_RV,
      cc.OP_FLOAT: self.float_toZ3_RV,
      cc.OP_ATOM_HEAD: self.atom_head_toZ3_RV,
      cc.OP_ATOM_TAIL: self.atom_tail_toZ3_RV,
      cc.OP_LIST_TO_TUPLE: self.list_to_tuple_toZ3_RV,
      cc.OP_TUPLE_TO_LIST: self.tuple_to_list_toZ3_RV,
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
      self.List.is_nil(self.Term.lval(t))
    ]
    self.axs.append(Not(And(*xs)))
  
  # Not a List
  def list_not_lst_toZ3_RV(self, term):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_lst(t))
  
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
    ax1 = self.Term.is_tpl(t)
    xs = []
    t = self.Term.tval(t)
    for i in range(0, n):
      xs.append(self.List.is_cons(t))
      t = self.List.tl(t)
    xs.append(t == self.List.nil)
    self.axs.append(Not(And(
      Not(And(*xs)),
      ax1
    )))
  
  # Not a Tuple (Reversed)
  def tuple_not_tpl_toZ3_RV(self, term, num):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_tpl(t))
  
  # ----------------------------------------------------------------------
  # Other Important Commands
  # ----------------------------------------------------------------------
  
  # Entry Point MFA's symbolic parameters
  def params_toZ3(self, *args):
    e = self.env
    for x in args:
      s = x["s"]
      p = e.fresh_var(s, self.Term)
      e.add_param(s)
  
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
  
  # (Reversed)
  def plus_toZ3_RV(self, term, term1, term2):
    T = self.Term
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(
      Not(And(
        Or(T.is_int(t1), T.is_real(t1)),
        Or(T.is_int(t2), T.is_real(t2))
      ))
    )
  
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
  
  # (Reversed)
  def minus_toZ3_RV(self, term, term1, term2):
    T = self.Term
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(
      Not(And(
        Or(T.is_int(t1), T.is_real(t1)),
        Or(T.is_int(t2), T.is_real(t2))
      ))
    )
  
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
  
  # (Reversed)
  def times_toZ3_RV(self, term, term1, term2):
    T = self.Term
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(
      Not(And(
        Or(T.is_int(t1), T.is_real(t1)),
        Or(T.is_int(t2), T.is_real(t2))
      ))
    )
  
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
  
  # (Reversed)
  def rdiv_toZ3_RV(self, term, term1, term2):
    T = self.Term
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(
      Not(And(
        Or(T.is_int(t1), T.is_real(t1)),
        Or(
          And(T.is_int(t2), T.ival(t2) != 0),
          And(T.is_real(t2), T.rval(t2) != 0.0)
        ),
      ))
    )
  
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
  
  # (Reversed)
  def idiv_nat_toZ3_RV(self, term, term1, term2):
    T = self.Term
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(
      Or(
        Not(T.is_int(t1)),
        Not(T.is_int(t2)),
        And( T.is_int(t1), T.is_int(t2), T.ival(t2) == 0 )
      )
    )
  
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
  
  # (Reversed)
  def rem_nat_toZ3_RV(self, term, term1, term2):
    T = self.Term
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(
      Or(
        Not(T.is_int(t1)),
        Not(T.is_int(t2)),
        And( T.is_int(t1), T.is_int(t2), T.ival(t2) == 0 )
      )
    )
  
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
  
  # (Reversed)
  def float_toZ3_RV(self, term, term1):
    T = self.Term
    t1 = self.term_toZ3(term1)
    self.axs.append(Not(Or(
      T.is_int(t1),
      T.is_real(t1)
    )))
  
  ## Convert a list to a tuple
  
  def list_to_tuple_toZ3(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(T.is_lst(t1))
    self.env.bind(s, T.tpl(T.lval(t1)))
  
  # (Reversed)
  def list_to_tuple_toZ3_RV(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(Not(T.is_lst(t1)))
  
  ## Convert a tuple to a list
  
  def tuple_to_list_toZ3(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(T.is_tpl(t1))
    self.env.bind(s, T.lst(T.tval(t1)))
  
  # (Reversed)
  def tuple_to_list_toZ3_RV(self, term, term1):
    T = self.Term
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(Not(T.is_tpl(t1)))
  
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
  
  # Reversed
  def atom_head_toZ3_RV(self, term, term1):
    T = self.Term
    A = self.Atom
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(Not(And(
      T.is_atm(t1),
      A.is_acons(T.aval(t1))
    )))
  
  
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
  
  # Reversed
  def atom_tail_toZ3_RV(self, term, term1):
    T = self.Term
    A = self.Atom
    s = term["s"]
    t1 = self.term_toZ3(term1)
    self.axs.append(Not(And(
      T.is_atm(t1),
      A.is_acons(T.aval(t1))
    )))
