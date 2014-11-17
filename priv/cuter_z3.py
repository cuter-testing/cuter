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
    return self.eZ3.Term.real(val)
  
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
      cc.OP_GUARD_TRUE: self.cmd_guard_true_toZ3,
      cc.OP_GUARD_FALSE: self.cmd_guard_false_toZ3,
      cc.OP_MATCH_EQUAL_TRUE: self.cmd_match_equal_true_toZ3,
      cc.OP_MATCH_EQUAL_FALSE: self.cmd_match_equal_false_toZ3,
      cc.OP_TUPLE_SZ: self.cmd_tuple_sz_toZ3,
      cc.OP_TUPLE_NOT_SZ: self.cmd_tuple_notsz_toZ3,
      cc.OP_TUPLE_NOT_TPL: self.cmd_tuple_nottpl_toZ3,
      cc.OP_LIST_NON_EMPTY: self.cmd_list_nonempty_toZ3,
      cc.OP_LIST_EMPTY: self.cmd_list_empty_toZ3,
      cc.OP_LIST_NOT_LST: self.cmd_list_notlst_toZ3,
      # Other important commands
      cc.OP_PARAMS: self.cmd_params_toZ3,
      cc.OP_UNFOLD_TUPLE: self.cmd_unfold_tuple_toZ3,
      cc.OP_UNFOLD_LIST: self.cmd_unfold_list_toZ3,
      # Erlang BIFs
      cc.OP_ERLANG_HD_1: self.cmd_erlang_hd_1_toZ3,
      cc.OP_ERLANG_TL_1: self.cmd_erlang_tl_1_toZ3,
      cc.OP_ERLANG_IS_INTEGER_1: self.cmd_erlang_isint_1_toZ3,
      cc.OP_ERLANG_IS_ATOM_1: self.cmd_erlang_isatom_1_toZ3,
      cc.OP_ERLANG_IS_FLOAT_1: self.cmd_erlang_isfloat_1_toZ3,
    }
    
    opts_rev = {
      # Constraints
      cc.OP_GUARD_TRUE: self.cmd_guard_false_toZ3,
      cc.OP_GUARD_FALSE: self.cmd_guard_true_toZ3,
      cc.OP_MATCH_EQUAL_TRUE: self.cmd_match_equal_false_toZ3,
      cc.OP_MATCH_EQUAL_FALSE: self.cmd_match_equal_true_toZ3,
      cc.OP_TUPLE_SZ: self.cmd_tuple_sz_toZ3_RV,
      cc.OP_TUPLE_NOT_SZ: self.cmd_tuple_notsz_toZ3_RV,
      cc.OP_TUPLE_NOT_TPL: self.cmd_tuple_nottpl_toZ3_RV,
      cc.OP_LIST_NON_EMPTY: self.cmd_list_nonempty_toZ3_RV,
      cc.OP_LIST_EMPTY: self.cmd_list_empty_toZ3_RV,
      cc.OP_LIST_NOT_LST: self.cmd_list_notlst_toZ3_RV,
      # Erlang BIFs
      cc.OP_ERLANG_HD_1: self.cmd_erlang_hd_1_toZ3_RV,
      cc.OP_ERLANG_TL_1: self.cmd_erlang_tl_1_toZ3_RV,
    }
    
    opts = opts_rev if rev else opts_normal
    opts[tp](*json_data["a"])
  
  # ----------------------------------------------------------------------
  # Constraints
  # ----------------------------------------------------------------------
  
  # Guard True
  def cmd_guard_true_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.append(t == self.atmTrue)
  
  # Guard False
  def cmd_guard_false_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.append(t == self.atmFalse)
  
  # NonEmpty List
  def cmd_list_nonempty_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.extend([
      self.Term.is_lst(t),
      self.List.is_cons(self.Term.lval(t))
    ])
  
  # Empty List
  def cmd_list_empty_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.extend([
      self.Term.is_lst(t),
      self.List.is_nil(self.Term.lval(t))
    ])
  
  # Not a List
  def cmd_list_notlst_toZ3(self, term):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_lst(t) == False)
  
  # Match Equal
  def cmd_match_equal_true_toZ3(self, term1, term2):
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(t1 == t2)
  
  # Match Not Equal
  def cmd_match_equal_false_toZ3(self, term1, term2):
    t1 = self.term_toZ3(term1)
    t2 = self.term_toZ3(term2)
    self.axs.append(t1 != t2)
  
  # Tuple of Size N
  def cmd_tuple_sz_toZ3(self, term, num):
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
  def cmd_tuple_notsz_toZ3(self, term, num):
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
  def cmd_tuple_nottpl_toZ3(self, term, num):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_tpl(t) == False)
  
  ### Reversed ###
  
  # NonEmpty List (Reversed)
  def cmd_list_nonempty_toZ3_RV(self, term):
    t = self.term_toZ3(term)
    xs = [
      self.Term.is_lst(t),
      self.List.is_cons(self.Term.lval(t)),
    ]
    self.axs.append(Not(And(*xs)))
  
  # Empty List (Reversed)
  def cmd_list_empty_toZ3_RV(self, term):
    t = self.term_toZ3(term)
    xs = [
      self.Term.is_lst(t),
      self.List.is_nil(self.Term.lval(t))
    ]
    self.axs.append(Not(And(*xs)))
  
  # Not a List
  def cmd_list_notlst_toZ3_RV(self, term):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_lst(t))
  
  # Tuple of Size N (Reversed)
  def cmd_tuple_sz_toZ3_RV(self, term, num):
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
  def cmd_tuple_notsz_toZ3_RV(self, term, num):
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
  def cmd_tuple_nottpl_toZ3_RV(self, term, num):
    t = self.term_toZ3(term)
    self.axs.append(self.Term.is_tpl(t))
  
  # ----------------------------------------------------------------------
  # Other Important Commands
  # ----------------------------------------------------------------------
  
  # Entry Point MFA's symbolic parameters
  def cmd_params_toZ3(self, *args):
    e = self.env
    for x in args:
      s = x["s"]
      p = e.fresh_var(s, self.Term)
      e.add_param(s)
  
  # Unfold a symbolic tuple
  def cmd_unfold_tuple_toZ3(self, *terms):
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
  def cmd_unfold_list_toZ3(self, *terms):
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
  # BIFs
  # ----------------------------------------------------------------------
  
  ### erlang:hd/1 ###
  
  def cmd_erlang_hd_1_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      self.Term.is_lst(t2),
      self.List.is_cons(self.Term.lval(t2))
    ])
    self.env.bind(s, self.List.hd(self.Term.lval(t2)))
  
  # (Reversed)
  def cmd_erlang_hd_1_toZ3_RV(self, term1, term2):
    t2 = self.term_toZ3(term2)
    self.axs.append(Or(
      self.Term.is_lst(t2) == False,
      And(self.Term.is_lst(t2), self.List.is_nil(self.Term.lval(t2)))
    ))
  
  ### erlang:tl/1 ###
  
  def cmd_erlang_tl_1_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.axs.extend([
      self.Term.is_lst(t2),
      self.List.is_cons(self.Term.lval(t2))
    ])
    self.env.bind(s, self.Term.lst(self.List.tl(self.Term.lval(t2))))
  
  # (Reversed)
  def cmd_erlang_tl_1_toZ3_RV(self, term1, term2):
    t2 = self.term_toZ3(term2)
    self.axs.append(Or(
      self.Term.is_lst(t2) == False,
      And(self.Term.is_lst(t2), self.List.is_nil(self.Term.lval(t2)))
    ))
  
  ### erlang:is_integer/1 ###
  
  def cmd_erlang_isint_1_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_int(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### erlang:is_atom/1 ###
  
  def cmd_erlang_isatom_1_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_atm(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
  ### erlang:is_float/1 ###
  
  def cmd_erlang_isfloat_1_toZ3(self, term1, term2):
    s = term1["s"]
    t2 = self.term_toZ3(term2)
    self.env.bind(s, If(
      self.Term.is_real(t2),
      self.atmTrue,
      self.atmFalse
    ))
  
