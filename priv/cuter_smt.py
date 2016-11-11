# -*- coding: utf-8 -*-

import cuter_generic_solver as cgs
import cuter_common as cc
import cuter_logger as clg
import smt


datatypes = [
	[
		"Term",
		["bool", ["bval", "Bool"]],
		["int", ["ival", "Int"]],
		["real", ["rval", "Real"]],
		["list", ["lval", "TList"]],
		["tuple", ["tval", "TList"]],
		["atom", ["aval", "IList"]],
	],
	[
		"TList",
		["nil"],
		["cons", ["hd", "Term"], ["tl", "TList"]],
	],
	[
		"IList",
		["inil"],
		["icons", ["ihd", "Int"], ["itl", "IList"]],
	],
]


false = [102, 97, 108, 115, 101]
true = [116, 114, 117, 101]


def calculate_int(obj):
	if isinstance(obj, list):
		if obj[0] == "-" and len(obj) == 2:
			i0 = calculate_int(obj[1])
			return -i0
	else:
		return int(obj)
	clg.debug_info("calculate_int: unknown operation " + str(obj))
	return None


def calculate_real(obj):
	if isinstance(obj, list):
		if obj[0] == "-" and len(obj) == 2:
			r0 = calculate_real(obj[1])
			return -r0
		if obj[0] == "/" and len(obj) == 3:
			r1 = calculate_real(obj[1])
			r2 = calculate_real(obj[2])
			return r1 / r2
	else:
		return float(obj)
	clg.debug_info("calculate_real: unknown operation " + str(obj))
	return None


def define_fun_rec(name, var, spec):
	return [
		"define-fun-rec",
		name,
		[[var, "Term"]],
		"Bool",
		[
			"and",
			["is-list", var],
			[
				"or",
				["is-nil", ["lval", var]],
				[
					"and",
					["is-cons", ["lval", var]],
					spec,
					[name, ["list", ["tl", ["lval", var]]]],
				],
			],
		]
	]


class ErlangSMT(cgs.AbstractErlangSolver):

	def __init__(self):
		self.vars = []
		self.aux_vars = []
		self.cmds = []
		self.cmds.append(["declare-datatypes", [], datatypes])
		self.solver = None
		self.model = None
		self.fn_cnt = 0
		self.solver = smt.SolverZ3()

	# =========================================================================
	# Public API.
	# =========================================================================

	def fix_parameter(self, p, v):
		"""
		Fixes a symbolic variable to a specific value.
		"""
		pass

	def reset_solver(self):
		"""
		Resets the solver.
		"""
		pass

	def add_axioms(self):
		"""
		Adds the axioms from memory to the solver.
		"""
		pass

	def solve(self):
		"""
		Solves a constraint set and returns the result.
		"""
		tpl = self.solver.solve(self.cmds, self.vars)
		self.model = tpl[1]
		result = tpl[0]
		if result == "sat":
			return cc.mk_sat()
		elif result == "unsat":
			return cc.mk_unsat()
		elif result == "unknown":
			return cc.mk_unknown()

	def encode_model(self):
		"""
		Encodes the resulting model.
		"""
		fn = lambda item: cc.mk_model_entry(cc.mk_symb(item[0][1:-1]), self.encode(item[1]))
		return cc.mk_model_data(cc.mk_model(map(fn, self.model)))

	# =========================================================================
	# Private Methods.
	# =========================================================================

	def assertion(self, assertion):
		self.cmds.append(["assert", assertion])

	def decode(self, data):
		"""
		Decodes a term to its SMT representation
		"""
		if cc.is_symb(data):
			s = "|{}|".format(cc.get_symb(data))
			if s not in self.vars and s not in self.aux_vars:
				self.aux_vars.append(s)
				self.cmds.append(["declare-const", s, "Term"])
			return s
		elif cc.is_int(data):
			return ["int", str(cc.get_int(data))]
		elif cc.is_float(data):
			return ["real", str(cc.get_float(data))]
		elif cc.is_atom(data):
			atom = cc.get_atom_chars(data)
			if atom == true:
				return ["bool", "true"]
			elif atom == false:
				return ["bool", "false"]
			else:
				return ["atom", self.value2ilist(atom)]
		elif cc.is_list(data):
			return ["list", self.value2tlist(cc.get_list_subterms(data))]
		elif cc.is_tuple(data):
			return ["tuple", self.value2tlist(cc.get_tuple_subterms(data))]
		else:
			clg.debug_info("decoding failed: " + str(data))
			return None # TODO decode term

	def value2tlist(self, value):
		if not value:
			return "nil"
		else:
			return ["cons", self.decode(value[0]), self.value2tlist(value[1:])]

	def value2ilist(self, value):
		if not value:
			return "inil"
		else:
			return ["icons", str(value[0]), self.value2ilist(value[1:])]

	def encode(self, data, table = {}):
		if data[0] == "bool":
			if data[1] == "true":
				return cc.mk_atom(true)
			else:
				return cc.mk_atom(false)
		elif data[0] == "int":
			return cc.mk_int(calculate_int(data[1]))
		elif data[0] == "real":
			return cc.mk_float(calculate_real(data[1]))
		elif data[0] == "atom":
			node = data[1]
			v = []
			while node != "inil":
				if isinstance(node, str) and node in table:
					node = table[node]
				if isinstance(node[1], str) and node[1] in table:
					node[1] = table[node[1]]
				v.append(int(node[1]))
				node = node[2]
			return cc.mk_atom(v)
		elif data[0] == "list":
			node = data[1]
			v = []
			while node != "nil":
				if isinstance(node, str) and node in table:
					node = table[node]
				if isinstance(node[1], str) and node[1] in table:
					node[1] = table[node[1]]
				v.append(self.encode(node[1], table))
				node = node[2]
			return cc.mk_list(v)
		elif data[0] == "tuple":
			node = data[1]
			v = []
			while node != "nil":
				if isinstance(node, str) and node in table:
					node = table[node]
				if isinstance(node[1], str) and node[1] in table:
					node[1] = table[node[1]]
				v.append(self.encode(node[1], table))
				node = node[2]
			return cc.mk_tuple(v)
		elif data[0] == "let":
			inner_table = table.copy()
			for var in data[1]:
				inner_table[var[0]] = var[1]
			ret = self.encode(data[2], inner_table)
			return ret
		clg.debug_info("encoding failed: " + str(data))
		return None # TODO encode term

	# -------------------------------------------------------------------------
	# Parse internal commands.
	# -------------------------------------------------------------------------

	def mfa_params(self, *args):
		"""
		Stores the entry point MFA's symbolic parameters.
		"""
		self.vars = []
		for arg in args:
			s = "|{}|".format(cc.get_symb(arg))
			self.vars.append(s)
			self.cmds.append(["declare-const", s, "Term"])

	def mfa_spec(self, spec):
		"""
		Stores the spec of the entry point MFA.
		"""
		p = cc.get_spec_clauses(spec)[0]
		pms = cc.get_parameters_from_complete_funsig(p)
		for item in zip(self.vars, pms):
			self.assertion(self.build_spec(item[1], item[0]))

	def build_spec(self, spec, var):
		if cc.is_type_any(spec):
			return "true"
		elif cc.is_type_float(spec):
			return ["is-real", var]
		elif cc.is_type_integer(spec):
			return ["is-int", var]
		#elif cc.is_type_integerlit(spec):
		#	literal = cc.get_literal_from_integerlit(spec)
		#	return ["and", ["is-int", var], ["=", ["ival", var], literal.value]] # TODO literal.value
		elif cc.is_type_list(spec):
			inner_spec = self.build_spec(cc.get_inner_type_from_list(spec), ["hd", ["lval", "t"]])
			name = "fn{}".format(self.fn_cnt)
			self.fn_cnt += 1
			self.cmds.append(define_fun_rec(name, "t", inner_spec))
			return [name, var]
		elif cc.is_type_tupledet(spec):
			inner_types = cc.get_inner_types_from_tupledet(spec)
			ret = ["and", ["is-tuple", var]]
			tlist = ["tval", var]
			for inner_type in inner_types:
				ret.append(["and", ["is-cons", tlist], self.build_spec(inner_type, ["hd", tlist])])
				tlist = ["tl", tlist]
			ret.append(["is-nil", tlist])
			return ret
		elif cc.is_type_union(spec):
			ret = ["or"]
			for inner_type in cc.get_inner_types_from_union(spec):
				ret.append(self.build_spec(inner_type, var))
			return ret
		elif cc.is_type_range(spec):
			ret = ["and", ["is-int", var]]
			limits = cc.get_range_bounds_from_range(spec)
			if cc.has_lower_bound(limits):
				ret.append([">=", ["ival", var], str(cc.get_lower_bound(limits))])
			if cc.has_upper_bound(limits):
				ret.append(["<=", ["ival", var], str(cc.get_upper_bound(limits))])
			return ret
		elif cc.is_type_nonempty_list(spec):
			inner_spec = self.build_spec(cc.get_inner_type_from_nonempty_list(spec), ["hd", ["lval", "t"]])
			name = "fn{}".format(self.fn_cnt)
			self.fn_cnt += 1
			self.cmds.append(define_fun_rec(name, "t", inner_spec))
			return ["and", ["is-list", var], ["is-cons", ["lval", var]], [name, var]]
		elif cc.is_type_atom(spec):
			return ["is-atom", var]
		clg.debug_info("unknown spec: " + str(spec))

	def unfold_tuple(self, *terms):
		"""
		Unfolds a symbolic tuple.
		"""
		t = self.decode(terms[0])
		self.assertion(["is-tuple", t])
		c = ["tval", t]
		for x in terms[1:]:
			self.assertion(["is-cons", c])
			s = "|{}|".format(cc.get_symb(x))
			if s not in self.vars and s not in self.aux_vars:
				self.aux_vars.append(s)
				self.cmds.append(["declare-const", s, "Term"])
			self.assertion(["=", s, ["hd", c]])
			c = ["tl", c]
		self.assertion(["is-nil", c])

	# -------------------------------------------------------------------------
	# Constraints.
	# -------------------------------------------------------------------------

	def guard_true(self, term):
		"""
		Asserts the predicate: term == true
		"""
		t = self.decode(term)
		self.assertion(["=", t, ["bool", "true"]])

	def guard_true_reversed(self, term):
		"""
		Asserts the predicate: Not (term == true)
		"""
		t = self.decode(term)
		self.assertion(["not", ["=", t, ["bool", "true"]]])

	def guard_false(self, term):
		"""
		Asserts the predicate: term == false
		"""
		t = self.decode(term)
		self.assertion(["=", t, ["bool", "false"]])

	def guard_false_reversed(self, term):
		"""
		Asserts the predicate: Not (term == false)
		"""
		t = self.decode(term)
		self.assertion(["not", ["=", t, ["bool", "false"]]])

	def match_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 == term2
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertion(["=", t1, t2])

	def match_equal_reversed(self, term1, term2):
		"""
		Asserts the predicate: Not (term1 == term2)
		"""
		self.match_not_equal(term1, term2)

	def match_not_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 != term2
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertion(["not", ["=", t1, t2]])

	def match_not_equal_reversed(self, term1, term2):
		"""
		Asserts the predicate: Not (term1 != term2)
		"""
		self.match_equal(term1, term2)

	def list_nonempty(self, term):
		"""
		Asserts that: term is a nonempty list.
		"""
		t = self.decode(term)
		self.assertion(["and", ["is-list", t], ["is-cons", ["lval", t]]])

	def list_nonempty_reversed(self, term):
		"""
		Asserts that: Not(term is a nonempty list).
		"""
		t = self.decode(term)
		self.assertion(["not", ["and", ["is-list", t], ["is-cons", ["lval", t]]]])

	def list_empty(self, term):
		"""
		Asserts that: term is an empty list.
		"""
		t = self.decode(term)
		self.assertion(["=", t, ["list", "nil"]])

	def list_empty_reversed(self, term):
		"""
		Asserts that: Not(term is an empty list).
		"""
		t = self.decode(term)
		self.assertion(["not", ["=", t, ["list", "nil"]]])

	def list_not_lst(self, term):
		"""
		Asserts that: term is not list.
		"""
		t = self.decode(term)
		self.assertion(["not", ["is-list", t]])

	def list_not_lst_reversed(self, term):
		"""
		Asserts that: Not (term is not list).
		"""
		t = self.decode(term)
		self.assertion(["is-list", t])

	def tuple_sz(self, term, num):
		"""
		Asserts that: term is a tuple of size num.
		"""
		t = self.decode(term)
		n = cc.get_int(num)
		l = ["and", ["is-tuple", t]]
		c = ["tval", t]
		while n > 0:
			l.append(["is-cons", c])
			c = ["tl", c]
			n -= 1
		l.append(["is-nil", c])
		self.assertion(l)

	def tuple_sz_reversed(self, term, num):
		"""
		Asserts that: term is not a tuple of size num.
		"""
		self.tuple_not_sz(term, num)

	def tuple_not_sz(self, term, num):
		"""
		Asserts that: term is not a tuple of size num.
		"""
		t = self.decode(term)
		n = cc.get_int(num)
		l = ["and", ["is-tuple", t]]
		c = ["tval", t]
		while n > 0:
			l.append(["is-cons", c])
			c = ["tl", c]
			n -= 1
		l.append(["is-nil", c])
		self.assertion(["not", l])

	def tuple_not_sz_reversed(self, term, num):
		"""
		Asserts that: Not (term is not a tuple of size num).
		"""
		self.tuple_sz(term, num)

	# -------------------------------------------------------------------------
	# Erlang BIFs or MFAs treated as BIFs.
	# -------------------------------------------------------------------------

	def head(self, term0, term1):
		"""
		Asserts that: term0 == hd(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["is-list", t1])
		self.assertion(["is-cons", ["lval", t1]])
		self.assertion(["=", t0, ["hd", ["lval", t1]]])

	def tail(self, term0, term1):
		"""
		Asserts that: term0 == tl(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["is-list", t1])
		self.assertion(["is-cons", ["lval", t1]])
		self.assertion(["=", t0, ["list", ["tl", ["lval", t1]]]])

	def cons(self, term0, term1, term2):
		"""
		Asserts that: term0 = [term1 | term2].
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertion(["is-list", t2])
		self.assertion(["=", t0, ["list", ["cons", t1, ["lval", t2]]]])

	def is_integer(self, term0, term1):
		"""
		Asserts that: term0 == is_integer(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["=", t0, ["bool", ["is-int", t1]]])

	def is_float(self, term0, term1):
		"""
		Asserts that: term1 == is_float(term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["=", t0, ["bool", ["is-real", t1]]])

	def is_list(self, term0, term1):
		"""
		Asserts that: term0 == is_list(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["=", t0, ["bool", ["is-list", t1]]])

	def is_tuple(self, term0, term1):
		"""
		Asserts that: term0 == is_tuple(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["=", t0, ["bool", ["is-tuple", t1]]])

	def is_number(self, term0, term1):
		"""
		Asserts that: term0 == is_number(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["=", t0, ["bool", ["or", ["is-int", t1], ["is-real", t1]]]])

	def _binary_operation(self, operator, term0, term1, term2):
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertion(["or", ["is-int", t1], ["is-real", t1]])
		self.assertion(["or", ["is-int", t2], ["is-real", t2]])
		self.assertion([
			"ite",
			["and", ["is-int", t1], ["is-int", t2]],
			["=", t0, ["int", [operator, ["ival", t1], ["ival", t2]]]],
			[
				"=",
				t0,
				[
					"real",
					[
						operator,
						["ite", ["is-int", t1], ["to_real", ["ival", t1]], ["rval", t1]],
						["ite", ["is-int", t2], ["to_real", ["ival", t2]], ["rval", t2]]
					]
				]
			]
		])

	def plus(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 + term2.
		"""
		self._binary_operation("+", term0, term1, term2)

	def minus(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 - term2.
		"""
		self._binary_operation("-", term0, term1, term2)

	def times(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 * term2.
		"""
		self._binary_operation("*", term0, term1, term2)

	def trunc(self, term0, term1):
		"""
		Asserts that: term0 is term1 truncated.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["or", ["is-int", t1], ["is-real", t1]])
		self.assertion([
			"=",
			t0,
			[
				"ite",
				["is-int", t1],
				t1,
				[
					"int",
					[
						"ite",
						[">=", ["rval", t1], "0.0"],
						["to_int", ["rval", t1]],
						["-", ["to_int", ["-", ["rval", t1]]]]
					]
				]
			]
		])

	def equal(self, term0, term1, term2):
		"""
		Asserts that: term0 = (term1 == term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertion(["=", t0, ["bool", ["=", t1, t2]]])

	def lt_integers(self, term0, term1, term2):
		"""
		Asserts that: term0 = (term1 < term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertion(["is-int", t1])
		self.assertion(["is-int", t2])
		self.assertion(["=", t0, ["bool", ["<", ["ival", t1], ["ival", t2]]]])

	def lt_floats(self, term0, term1, term2):
		"""
		Asserts that: term0 = (term1 < term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertion(["is-real", t1])
		self.assertion(["is-real", t2])
		self.assertion(["=", t0, ["bool", ["<", ["rval", t1], ["rval", t2]]]])

	def to_float(self, term0, term1):
		"""
		Asserts that: term0 = float(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.assertion(["or", ["is-int", t1], ["is-real", t1]])
		self.assertion(["=", t0, ["real", ["ite", ["is-int", t1], ["to_real", ["ival", t1]], ["rval", t1]]]])
