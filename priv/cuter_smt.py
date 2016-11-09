# -*- coding: utf-8 -*-

import cuter_generic_solver as cgs
import cuter_common as cc
#import cuter_logger as clg
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
	smt.log("calculate_int: unknown operation " + str(obj))
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
	smt.log("calculate_real: unknown operation " + str(obj))
	return None


def build_spec(spec, var):
	#smt.log("spec: " + str(spec))
	tp = spec["tp"]
	if tp == cc.JSON_ERLTYPE_ANY:
		return "true"
	elif tp == cc.JSON_ERLTYPE_FLOAT:
		return ["is-real", var]
	elif tp == cc.JSON_ERLTYPE_INTEGER:
		return ["is-int", var]
	elif tp == cc.JSON_ERLTYPE_LIST:
		# sample spec {u'a': {u'tp': 4}, u'tp': 6}
		# TODO type of list elements
		return ["is-list", var]
	elif tp == cc.JSON_ERLTYPE_UNION:
		ret = ["or"]
		for item in spec["a"]:
			ret.append(build_spec(item, var))
		return ret
	elif tp == cc.JSON_ERLTYPE_RANGE:
		ret = ["and", ["is-int", var]]
		limits = spec["a"]
		if not("tp" in limits[0] and limits[0]["tp"] == cc.JSON_ERLTYPE_INTEGER) and limits[0]["t"] == cc.JSON_TYPE_INT:
			ret.append([">=", ["ival", var], str(limits[0]["v"])])
		if not("tp" in limits[1] and limits[1]["tp"] == cc.JSON_ERLTYPE_INTEGER) and limits[1]["t"] == cc.JSON_TYPE_INT:
			ret.append(["<=", ["ival", var], str(limits[1]["v"])])
		return ret
	smt.log("unknown spec: " + str(spec))


class ErlangSMT(cgs.AbstractErlangSolver):

	def __init__(self):
		self.vars = []
		self.aux_vars = []
		self.cmds = []
		self.cmds.append(["declare-datatypes", [], datatypes])
		self.solver = None
		self.model = None
		self.solver = smt.SolverZ3()
		smt.log("***")

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
		return tpl[0]

	def encode_model(self):
		"""
		Encodes the resulting model to JSON.
		"""
		return [({"s": item[0][1:-1]}, self.encode(item[1])) for item in self.model]

	# =========================================================================
	# Private Methods.
	# =========================================================================

	def assertion(self, assertion):
		#smt.log("assertion " + str(assertion))
		self.cmds.append(["assert", assertion])

	def decode(self, data):
		"""
		Decodes a JSON term to its SMT representation
		"""
		if "s" in data:
			if data["s"] not in self.vars and data["s"] not in self.aux_vars:
				var = data["s"]
				self.aux_vars.append(var)
				self.cmds.append(["declare-const", "|{}|".format(var), "Term"])
			return "|{}|".format(data["s"])
		elif data["t"] == cc.JSON_TYPE_INT:
			return ["int", str(data["v"])]
		elif data["t"] == cc.JSON_TYPE_FLOAT:
			return ["real", str(data["v"])]
		elif data["t"] == cc.JSON_TYPE_ATOM:
			if data["v"] == true:
				return ["bool", "true"]
			elif data["v"] == false:
				return ["bool", "false"]
			else:
				return ["atom", self.value2ilist(data["v"])]
		elif data["t"] == cc.JSON_TYPE_LIST:
			return ["list", self.value2tlist(data["v"])]
		elif data["t"] == cc.JSON_TYPE_TUPLE:
			return ["tuple", self.value2tlist(data["v"])]
		else:
			smt.log("decoding failed: " + str(data))
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
				return {"t": cc.JSON_TYPE_ATOM, "v": true}
			else:
				return {"t": cc.JSON_TYPE_ATOM, "v": false}
		elif data[0] == "int":
			return {"t": cc.JSON_TYPE_INT, "v": calculate_int(data[1])}
		elif data[0] == "real":
			return {"t": cc.JSON_TYPE_FLOAT, "v": calculate_real(data[1])}
		elif data[0] == "atom":
			node = data[1]
			v = []
			while True:
				if node == "inil":
					break
				v.append(int(node[1]))
				node = node[2]
			return {"t": cc.JSON_TYPE_ATOM, "v": v}
		elif data[0] == "list":
			node = data[1]
			v = []
			while True:
				if isinstance(node, str) and node in table:
					node = table[node]
				if node == "nil":
					break
				v.append(self.encode(node[1], table))
				node = node[2]
			return {"t": cc.JSON_TYPE_LIST, "v": v}
		elif data[0] == "let":
			inner_table = table.copy()
			for var in data[1]:
				inner_table[var[0]] = var[1]
			ret = self.encode(data[2], inner_table)
			return ret
		smt.log("encoding failed: " + str(data))
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
			var = arg["s"]
			self.vars.append(var)
			self.cmds.append(["declare-const", "|{}|".format(var), "Term"])

	def mfa_spec(self, *spec):
		"""
		Stores the spec of the entry point MFA.
		"""
		p = spec[0]["p"]
		for item in zip(self.vars, p):
			self.assertion(build_spec(item[1], "|{}|".format(item[0])))
		return # TODO clear code
		for item in zip(self.vars, p):
			var = "|{}|".format(item[0])
			tp = item[1]["tp"]
			if tp == cc.JSON_ERLTYPE_LIST:
				smt.log("list item: " + str(item)) # TODO list spec
				#self.cmds.append([
				#	"define-fun-rec", "list_int", [["t", "Term"]], "Bool",
				#	[
				#		"or",
				#		["is-nil", ["lval", "t"]],
				#		[
				#			"and",
				#			["is-cons", ["lval", "t"]],
				#			["is-int", ["hd", ["lval", "t"]]],
				#			["list_int", ["list", ["tl", ["lval", "t"]]]],
				#		],
				#	]
				#])
				#self.assertion(["list_int", var])
				# OR
				# (assert (forall ((t Term)) (= (list_int t) (or (is-nil (lval t)) (and (is-cons (lval t)) (is-int (hd (lval t))) (list_int (list (tl (lval t)))))))))
			else:
				smt.log("item: " + str(item)) # TODO spec

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
		t1 = self.decode(term1);
		t2 = self.decode(term2);
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
