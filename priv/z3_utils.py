import json
from z3 import *

class Env:
  def __init__(self):
    self.cnt = 0
    self.e = {}
    self.params = []
  
  def add_param(self, x):
    self.params.append(x)
  
  def lookup(self, x):
    if (x in self.e):
      return self.e[x]
    else:
      return None
  
  def fresh_var(self, s, Type):
    self.cnt += 1
    x =  Const("x%s" % self.cnt, Type)
    self.e[s] = x
    return x

class ErlangZ3:
  def __init__(self):
    self.Term, self.List, self.Atom = self.erlang_types()
    self.env = Env()
    self.solver = Solver()
    self.atom_true = self.json_term_to_z3(json.loads("{\"t\" : \"Atom\", \"v\" : [116,114,117,101]}"))
    self.atom_false = self.json_term_to_z3(json.loads("{\"t\" : \"Atom\", \"v\" : [102,97,108,115,101]}"))
    self.check = None
    self.model = None
  
  ## Solve a Constraint Set
  def solve(self):
    self.check = self.solver.check()
    if (self.check == sat):
      self.model = self.solver.model()
      return True
    else:
      return False
  
  ## Define the Erlang Type System
  def erlang_types(*args):
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
  
  ## Encode an Erlang term in JSON representation to Z3
  def json_term_to_z3(self, json_data):
    if ("s" in json_data):
      return self._json_symbolic_term_to_z3(json_data)
    else:
      d = json_data["d"] if ("d" in json_data) else {}
      return self._json_concrete_term_to_z3(json_data, d)
  
  def _json_symbolic_term_to_z3(self, json_data):
    e = self.env
    s = json_data["s"]
    v = e.lookup(s)
    if (v is not None):
      return v
    else:
      x = e.fresh_var(s, self.Term)
      return x
  
  def _json_concrete_term_to_z3(self, json_data, d):
    e = self.env
    if ("l" in json_data):
      return self._json_alias_term_to_z3(json_data, d)
    else:
      opts = {
        "Int" : self._json_int_term_to_z3,
        "Real" : self._json_real_term_to_z3,
        "List" : self._json_list_term_to_z3,
        "Tuple" : self._json_tuple_term_to_z3,
        "Atom" : self._json_atom_term_to_z3,
      }
      return opts[json_data["t"]](json_data["v"], d)
  
  
  def _json_int_term_to_z3(self, val, d):
    return self.Term.int(val)
  
  def _json_real_term_to_z3(self, val, d):
    return self.Term.real(val)
  
  def _json_list_term_to_z3(self, val, d):
    val.reverse()
    term = self.List.nil
    while val != []:
      head, val = val[0], val[1:]
      enc_head = self._json_concrete_term_to_z3(head, d)
      term = self.List.cons(enc_head, term)
    return self.Term.lst(term)
  
  def _json_tuple_term_to_z3(self, val, d):
    val.reverse()
    term = self.List.nil
    while val != []:
      head, val = val[0], val[1:]
      enc_head = self._json_concrete_term_to_z3(head, d)
      term = self.List.cons(enc_head, term)
    return self.Term.tpl(term)
  
  def _json_atom_term_to_z3(self, val, d):
    val.reverse()
    term = self.Atom.anil
    while val != []:
      head, val = val[0], val[1:]
      term = self.Atom.acons(head, term)
    term = self.Term.atm(term)
    return term
  
  def _json_alias_term_to_z3(self, json_data, d):
    e = self.env
    s = json_data["l"]
    v = e.lookup(s)
    if (v is not None):
      return v
    else:
      x = e.fresh_var(s, self.Term)
      y = self._json_concrete_term_to_z3(d[s], d)
      self.solver.add(x == y)
      return x
  
  ## Decode a Z3 object to an Erlang term in JSON representation
  def z3_term_to_json(self, term):
    T = self.Term
    if (is_true(simplify(T.is_int(term)))):
      return self._z3_int_term_to_json(T.ival(term))
    elif (is_true(simplify(T.is_real(term)))):
      return self._z3_real_term_to_json(T.rval(term))
    elif (is_true(simplify(T.is_lst(term)))):
      return self._z3_list_term_to_json(T.lval(term))
    elif (is_true(simplify(T.is_tpl(term)))):
      return self._z3_tuple_term_to_json(T.tval(term))
    elif (is_true(simplify(T.is_atm(term)))):
      return self._z3_atom_term_to_json(T.aval(term))
  
  def _z3_int_term_to_json(self, t):
    return {"t" : "Int", "v" : simplify(t).as_long()}
  
  def _z3_real_term_to_json(self, t):
    s = simplify(t)
    f = float(s.numerator_as_long()) / float(s.denominator_as_long())
    return {"t" : "Real", "v" : f}
  
  def _z3_list_term_to_json(self, t):
    L = self.List
    s = simplify(t)
    r = []
    while (is_true(simplify(L.is_cons(s)))):
      hd = simplify(L.hd(s))
      s = simplify(L.tl(s))
      r.append(self.z3_term_to_json(hd))
    return {"t" : "List", "v" : r}
  
  def _z3_tuple_term_to_json(self, t):
    L = self.List
    s = simplify(t)
    r = []
    while (is_true(simplify(L.is_cons(s)))):
      hd = simplify(L.hd(s))
      s = simplify(L.tl(s))
      r.append(self.z3_term_to_json(hd))
    return {"t" : "Tuple", "v" : r}
  
  def _z3_atom_term_to_json(self, t):
    A = self.Atom
    s = simplify(t)
    r = []
    while (is_true(simplify(A.is_acons(s)))):
      hd = simplify(A.ahd(s))
      s = simplify(A.atl(s))
      r.append(simplify(hd).as_long())
    return {"t" : "Atom", "v" : r}
  
  ## Encode Commands in JSON representation to Z3
  def json_command_to_z3(self, json_data):
    opts = {
      # Constraint Commands
      "Eq" : self._json_cmd_eq_to_z3,
      "Neq" : self._json_cmd_neq_to_z3,
      "T" : self._json_cmd_true_to_z3,
      "F" : self._json_cmd_false_to_z3,
      "Nel" : self._json_cmd_nel_to_z3,
      "El" : self._json_cmd_el_to_z3,
      "Nl" : self._json_cmd_nl_to_z3,
      "Nt" : self._json_cmd_nt_to_z3,
      "Ts" : self._json_cmd_ts_to_z3,
      "Nts" : self._json_cmd_nts_to_z3,
      # Operator Commands
      "=:=" : self._json_bif_seq_to_z3,
      "=/=" : self._json_bif_sneq_to_z3,
      "+" : self._json_bif_add_to_z3,
      "-" : self._json_bif_minus_to_z3,
      "*" : self._json_bif_mult_to_z3,
      "/" : self._json_bif_rdiv_to_z3,
      "div" : self._json_bif_div_to_z3,
      "rem" : self._json_bif_rem_to_z3,
      "or" : self._json_bif_or_to_z3,
      "and" : self._json_bif_and_to_z3,
      "ore" : self._json_bif_orelse_to_z3,
      "anda" : self._json_bif_andalso_to_z3,
      "not" : self._json_bif_not_to_z3,
      "<" : self._json_bif_lt_to_z3,
      ">" : self._json_bif_gt_to_z3,
      ">=" : self._json_bif_gteq_to_z3,
      "=<" : self._json_bif_lteq_to_z3,
      # BIF Commands
      "hd" : self._json_bif_hd_to_z3,
      "tl" : self._json_bif_tl_to_z3,
      "abs" : self._json_bif_abs_to_z3,
      "elm" : self._json_bif_elem_to_z3,
      "flt" : self._json_bif_float_to_z3,
      "isa" : self._json_bif_is_atom_to_z3,
      "isb" : self._json_bif_is_boolean_to_z3,
      "isf" : self._json_bif_is_float_to_z3,
      "isi" : self._json_bif_is_integer_to_z3,
      "isl" : self._json_bif_is_list_to_z3,
      "isn" : self._json_bif_is_number_to_z3,
      "ist" : self._json_bif_is_tuple_to_z3,
      "rnd" : self._json_bif_round_to_z3,
      "trc" : self._json_bif_trunc_to_z3,
      "ltt" : self._json_bif_list_to_tuple_to_z3,
      "ttl" : self._json_bif_tuple_to_list_to_z3,
      # Other Useful Commands
      "Pms" : self._json_cmd_define_params_to_z3,
      "Bkt" : self._json_cmd_break_tuple_to_z3,
      "Bkl" : self._json_cmd_break_list_to_z3,
    }
    opts_rev = {
      # Reversed Constraint Commands
      "Eq" : self._json_cmd_neq_to_z3,
      "Neq" : self._json_cmd_eq_to_z3,
      "T" : self._json_cmd_false_to_z3,
      "F" : self._json_cmd_true_to_z3,
      "Nel" : self._json_rev_cmd_nel_to_z3,
      "El" : self._json_cmd_nel_to_z3,
      "Nl" : self._json_rev_cmd_nl_to_z3,
      "Ts" : self._json_rev_cmd_ts_to_z3,
      "Nt" : self._json_cmd_ts_to_z3,
      "Nts" : self._json_cmd_ts_to_z3,
    }
    if ("r" in json_data):
      opts_rev[json_data["c"]](*json_data["a"])
    else:
      opts[json_data["c"]](*json_data["a"])
  
  # Constraints
  
  # "Equal"
  def _json_cmd_eq_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(t1 == t2)
  
  # "Not Equal"
  def _json_cmd_neq_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(t1 != t2)
  
  # "Guard True"
  def _json_cmd_true_to_z3(self, term):
    t = self.json_term_to_z3(term)
    self.solver.add(t == self.atom_true)
  
  # "Guard False"
  def _json_cmd_false_to_z3(self, term):
    t = self.json_term_to_z3(term)
    self.solver.add(t == self.atom_false)
  
  # "Non Empty List"
  def _json_cmd_nel_to_z3(self, term):
    s = self.solver
    t = self.json_term_to_z3(term)
    s.add(self.Term.is_lst(t))
    s.add(self.List.is_cons(self.Term.lval(t)))
  
  # Reversed "Non Empty List"
  def _json_rev_cmd_nel_to_z3(self, term):
    t = self.json_term_to_z3(term)
    e = And(
      self.Term.is_lst(t),
      self.List.is_cons(self.Term.lval(t))
    )
    self.solver.add(Not(e))
  
  # "Empty List"
  def _json_cmd_el_to_z3(self, term):
    t = self.json_term_to_z3(term)
    s.add(self.Term.is_lst(t))
    s.add(self.List.is_nil(self.Term.lval(t)))
  
  # "Not List"
  def _json_cmd_nl_to_z3(self, term):
    t = self.json_term_to_z3(term)
    self.solver.add(self.Term.is_lst(t) == False)
  
  # Reversed "Not List"
  def _json_rev_cmd_nl_to_z3(self, term):
    t = self.json_term_to_z3(term)
    self.solver.add(self.Term.is_lst(t))
  
  # "Not Tuple"
  def _json_cmd_nt_to_z3(self, term, l_json):
    t = self.json_term_to_z3(term)
    self.solver.add(self.Term.is_tpl(t) == False)
  
  # "Tuple of size N"
  def _json_cmd_ts_to_z3(self, term, l_json):
    s = self.solver
    t = self.json_term_to_z3(term)
    l = l_json["v"] # Expect l_json to represent an Integer
    s.add(self.Term.is_tpl(t))
    t = self.Term.tval(t)
    for i in range(0, l):
      s.add(self.List.is_cons(t))
      t = self.List.tl(t)
    s.add(t == self.List.nil)
  
  # Reversed "Tuple of size N"
  def _json_rev_cmd_ts_to_z3(self, term, l_json):
    t = self.json_term_to_z3(term)
    l = l_json["v"] # Expect l_json to represent an Integer
    e = [self.Term.is_tpl(t)]
    t = self.Term.tval(t)
    for i in range(0, l):
      e.append(self.List.is_cons(t))
      t = self.List.tl(t)
    e.append(t == self.List.nil)
    self.solver.add(Not(And(*e)))
  
  # "Tuple Not of size N"
  def _json_cmd_nts_to_z3(self, term, l_json):
    s = self.solver
    t = self.json_term_to_z3(term)
    l = l_json["v"] # Expect l_json to represent an Integer
    s.add(self.Term.is_tpl(t))
    preds = []
    t = self.Term.tval(t)
    for i in range(0, l):
      preds.append(self.List.is_cons(t))
      t = self.List.tl(t)
    preds.append(t == self.List.nil)
    s.add(Not(And(*preds)))
  
  # Other Useful Commands
  
  # Define Parameters
  def _json_cmd_define_params_to_z3(self, *args):
    for s in args:
      x = self._json_symbolic_term_to_z3(s)
      self.env.add_param(s["s"])
  
  # 'Break Tuple'
  def _json_cmd_break_tuple_to_z3(self, term1, terms):
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    s.add(self.Term.is_tpl(t1))
    t1 = self.Term.tval(t1)
    for term in terms:
      t = self.json_term_to_z3(term)
      s.add(self.List.is_cons(t1))
      s.add(t == self.List.hd(t1))
      t1 = self.List.tl(t1)
    s.add(t1 == self.List.nil)
  
  # 'Break List'
  def _json_cmd_break_list_to_z3(self, term1, terms):
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    s.add(self.Term.is_lst(t1))
    t1 = self.Term.lval(t1)
    for term in terms:
      t = self.json_term_to_z3(term)
      s.add(self.List.is_cons(t1))
      s.add(t == self.List.hd(t1))
      t1 = self.List.tl(t1)
    s.add(t1 == self.List.nil)
  
  # BIFs
  
  # erlang:hd/1
  def _json_bif_hd_to_z3(self, term1, term2):
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    s.add(self.Term.is_lst(t1))
    s.add(self.List.is_cons(self.Term.lval(t1)))
    s.add(self.List.hd(self.Term.lval(t1)) == t2)
  
  # erlang:tl/1
  def _json_bif_tl_to_z3(self, term1, term2):
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    s.add(self.Term.is_lst(t1))
    s.add(self.List.is_cons(self.Term.lval(t1)))
    s.add(self.Term.lst(self.List.tl(self.Term.lval(t1))) == t2)
  
  # erlang:'=:='/2
  def _json_bif_seq_to_z3(self, term1, term2, term3):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(If(t1 == t2, t3 == self.atom_true, t3 == self.atom_false))
  
  # erlang:'=/='/2
  def _json_bif_sneq_to_z3(self, term1, term2, term3):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(If(t1 != t2, t3 == self.atom_true, t3 == self.atom_false))
  
  # erlang:'+'/2
  def _json_bif_add_to_z3(self, term1, term2, term3):
    T = self.Term
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), T.is_int(t3), T.ival(t1) + T.ival(t2) == T.ival(t3)),
      And(T.is_int(t1), T.is_real(t2), T.is_real(t3), T.ival(t1) + T.rval(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_int(t2), T.is_real(t3), T.rval(t1) + T.ival(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_real(t2), T.is_real(t3), T.rval(t1) + T.rval(t2) == T.rval(t3))
    ))
  
  # erlang:'-'/2
  def _json_bif_minus_to_z3(self, term1, term2, term3):
    T = self.Term
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), T.is_int(t3), T.ival(t1) - T.ival(t2) == T.ival(t3)),
      And(T.is_int(t1), T.is_real(t2), T.is_real(t3), T.ival(t1) - T.rval(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_int(t2), T.is_real(t3), T.rval(t1) - T.ival(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_real(t2), T.is_real(t3), T.rval(t1) - T.rval(t2) == T.rval(t3))
    ))
  
  # erlang:'*'/2
  def _json_bif_mult_to_z3(self, term1, term2, term3):
    T = self.Term
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), T.is_int(t3), T.ival(t1) * T.ival(t2) == T.ival(t3)),
      And(T.is_int(t1), T.is_real(t2), T.is_real(t3), T.ival(t1) * T.rval(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_int(t2), T.is_real(t3), T.rval(t1) * T.ival(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_real(t2), T.is_real(t3), T.rval(t1) * T.rval(t2) == T.rval(t3))
    ))
  
  # erlang:'/'/2
  def _json_bif_rdiv_to_z3(self, term1, term2, term3):
    T = self.Term
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(T.is_real(t3))
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), T.ival(t2) != 0, T.ival(t1) / ToReal(T.ival(t2)) == T.rval(t3)),
      And(T.is_int(t1), T.is_real(t2), T.rval(t2) != 0.0, T.ival(t1) / T.rval(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_int(t2), T.ival(t2) != 0, T.rval(t1) / T.ival(t2) == T.rval(t3)),
      And(T.is_real(t1), T.is_real(t2), T.rval(t2) != 0.0, T.rval(t1) / T.rval(t2) == T.rval(t3))
    ))
  
  # erlang:'div'/2
  def _json_bif_div_to_z3(self, term1, term2, term3):
    s = self.solver
    T = self.Term
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    s.add(T.is_int(t1))
    s.add(T.is_int(t2))
    s.add(T.is_int(t3))
    s.add(T.ival(t2) != 0)
    s.add(T.ival(t1) / T.ival(t2) == T.ival(t3))
  
  # erlang:'rem'/2
  def _json_bif_rem_to_z3(self, term1, term2, term3):
    s = self.solver
    T = self.Term
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    s.add(T.is_int(t1))
    s.add(T.is_int(t2))
    s.add(T.is_int(t3))
    s.add(T.ival(t2) != 0)
    s.add(T.ival(t1) % T.ival(t2) == T.ival(t3))
  
  # erlang:abs/1
  def _json_bif_abs_to_z3(self, term1, term2):
    T = self.Term
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    iabs = Or(
      And(T.ival(t1) < 0, T.ival(t2) == -T.ival(t1)),
      And(T.ival(t1) >= 0, T.ival(t2) == T.ival(t1)),
    )
    rabs = Or(
      And(T.rval(t1) < 0.0, T.rval(t2) == -T.rval(t1)),
      And(T.rval(t1) >= 0.0, T.rval(t2) == T.rval(t1)),
    )
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), iabs),
      And(T.is_real(t1), T.is_real(t2), rabs),
    ))
  
  # erlang:'or'/2
  def _json_bif_or_to_z3(self, term1, term2, term3):
    T = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(t1 == T, t2 == F, t3 == T),
      And(t1 == F, t2 == T, t3 == T),
      And(t1 == T, t2 == T, t3 == T),
      And(t1 == F, t2 == F, t3 == F)
    ))
  
  # erlang:'and'/2
  def _json_bif_and_to_z3(self, term1, term2, term3):
    T = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(t1 == T, t2 == F, t3 == F),
      And(t1 == F, t2 == T, t3 == F),
      And(t1 == T, t2 == T, t3 == T),
      And(t1 == F, t2 == F, t3 == F)
    ))
  
  # erlang:'orelse'/2
  def _json_bif_orelse_to_z3(self, term1, term2, term3):
    T = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(t1 == T, t3 == T),
      And(t1 == F, t2 == T, t3 == T),
      And(t1 == F, t2 == F, t3 == F)
    ))
  
  # erlang:'andalso'/2
  def _json_bif_andalso_to_z3(self, term1, term2, term3):
    T = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(t1 == T, t2 == F, t3 == F),
      And(t1 == T, t2 == T, t3 == T),
      And(t1 == F, t3 == F)
    ))
  
  # erlang:'not'/2
  def _json_bif_not_to_z3(self, term1, term2):
    T = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(Or(
      And(t1 == T, t2 == F),
      And(t1 == F, t2 == T)
    ))
  
  # erlang:'<'/2
  #!# SIMPLIFIED (Only supports numbers)
  def _json_bif_lt_to_z3(self, term1, term2, term3):
    T = self.Term
    Tr = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), If(T.ival(t1) < T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_int(t1), T.is_real(t2), If(T.ival(t1) < T.rval(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_int(t2), If(T.rval(t1) < T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_real(t2), If(T.rval(t1) < T.rval(t2), t3 == Tr, t3 == F))
    ))
  
  # erlang:'>'/2
  #!# SIMPLIFIED (Only supports numbers)
  def _json_bif_gt_to_z3(self, term1, term2, term3):
    T = self.Term
    Tr = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), If(T.ival(t1) > T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_int(t1), T.is_real(t2), If(T.ival(t1) > T.rval(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_int(t2), If(T.rval(t1) > T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_real(t2), If(T.rval(t1) > T.rval(t2), t3 == Tr, t3 == F))
    ))
  
  # erlang:'>='/2
  #!# SIMPLIFIED (Only supports numbers)
  def _json_bif_gteq_to_z3(self, term1, term2, term3):
    T = self.Term
    Tr = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), If(T.ival(t1) >= T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_int(t1), T.is_real(t2), If(T.ival(t1) >= T.rval(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_int(t2), If(T.rval(t1) >= T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_real(t2), If(T.rval(t1) >= T.rval(t2), t3 == Tr, t3 == F))
    ))
  
  # erlang:'=<'/2
  #!# SIMPLIFIED (Only supports numbers)
  def _json_bif_lteq_to_z3(self, term1, term2, term3):
    T = self.Term
    Tr = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    self.solver.add(Or(
      And(T.is_int(t1), T.is_int(t2), If(T.ival(t1) <= T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_int(t1), T.is_real(t2), If(T.ival(t1) <= T.rval(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_int(t2), If(T.rval(t1) <= T.ival(t2), t3 == Tr, t3 == F)),
      And(T.is_real(t1), T.is_real(t2), If(T.rval(t1) <= T.rval(t2), t3 == Tr, t3 == F))
    ))
  
  # erlang:element/2
  #!# SIMPLIFIED (Expect term1 to represent an Integer)
  def _json_bif_elem_to_z3(self, term1, term2, term3):
    s = self.solver
    l = term1["v"] # Simplification
    t2 = self.json_term_to_z3(term2)
    t3 = self.json_term_to_z3(term3)
    s.add(self.Term.is_tpl(t2))
    t = self.Term.tval(t2)
    for i in range(0, l):
      s.add(self.List.is_cons(t))
      h = self.List.hd(t)
      t = self.List.tl(t)
    s.add(t3 == h)
  
  
  # erlang:float/1
  def _json_bif_float_to_z3(self, term1, term2):
    T = self.Term
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    s.add(T.is_real(t2))
    s.add(Or(
      And(T.is_real(t1), t2 == t1),
      And(T.is_int(t1), T.rval(t2) == ToReal(T.ival(t1)))
    ))
  
  # erlang:is_atom/1
  def _json_bif_is_atom_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(If(
      self.Term.is_atm(t1),
      t2 == self.atom_true,
      t2 == self.atom_false
    ))
  
  # erlang:is_float/1
  def _json_bif_is_float_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(If(
      self.Term.is_real(t1),
      t2 == self.atom_true,
      t2 == self.atom_false
    ))
  
  # erlang:is_integer/1
  def _json_bif_is_integer_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(If(
      self.Term.is_int(t1),
      t2 == self.atom_true,
      t2 == self.atom_false
    ))
  
  # erlang:is_list/1
  def _json_bif_is_list_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(If(
      self.Term.is_lst(t1),
      t2 == self.atom_true,
      t2 == self.atom_false
    ))
  
  # erlang:is_tuple/1
  def _json_bif_is_tuple_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(If(
      self.Term.is_tpl(t1),
      t2 == self.atom_true,
      t2 == self.atom_false
    ))
  
  # erlang:is_boolean/1
  def _json_bif_is_boolean_to_z3(self, term1, term2):
    T = self.atom_true
    F = self.atom_false
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(If(
      Or(t1 == T, t1 == F),
      t2 == T,
      t2 == F
    ))
  
  # erlang:is_number/1
  def _json_bif_is_number_to_z3(self, term1, term2):
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    self.solver.add(If(
      Or(self.Term.is_int(t1), self.Term.is_real(t1)),
      t2 == self.atom_true,
      t2 == self.atom_false
    ))
  
  # erlang:trunc/1
  def _json_bif_trunc_to_z3(self, term1, term2):
    T = self.Term
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    s.add(T.is_int(t2))
    e1 = And(T.is_int(t1), t2 == t1)
    e2 = And(T.is_real(t1), If(
      T.rval(t1) >= 0.0,
      T.ival(t2) == ToInt(T.rval(t1)),
      T.ival(t2) == ToInt(T.rval(t1)) + 1,
    ))
    s.add(Or(e1, e2))
  
  # erlang:round/1
  def _json_bif_round_to_z3(self, term1, term2):
    T = self.Term
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    s.add(T.is_int(t2))
    e1 = And(T.is_int(t1), t2 == t1)
    t = ToInt(T.rval(t1))
    e2 = And(T.is_real(t1), If(
      T.rval(t1) - t >= 0.5,
      T.ival(t2) == t + 1,
      T.ival(t2) == t
    ))
    s.add(Or(e1, e2))
  
  # erlang:list_to_tuple/1
  def _json_bif_list_to_tuple_to_z3(self, term1, term2):
    T = self.Term
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    s.add(T.is_lst(t1))
    s.add(T.is_tpl(t2))
    s.add(T.lval(t1) == T.tval(t2))
  
  # erlang:tuple_to_list/1
  def _json_bif_tuple_to_list_to_z3(self, term1, term2):
    T = self.Term
    s = self.solver
    t1 = self.json_term_to_z3(term1)
    t2 = self.json_term_to_z3(term2)
    s.add(T.is_tpl(t1))
    s.add(T.is_lst(t2))
    s.add(T.tval(t1) == T.lval(t2))
  
  ## Decode the Z3 solution to JSON
  def z3_solution_to_json(self):
    sol = {}
    for s in self.env.params:
      sol[s] = self.z3_param_to_json(s)
    return sol
  
  def z3_param_to_json(self, s):
    x = self.env.lookup(s)
    v = self.model[x]
    if (v is None):
      return "any"
    else:
      return self.z3_term_to_json(v)
      
  
