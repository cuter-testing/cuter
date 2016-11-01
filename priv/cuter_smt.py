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
	],
	[
		"TList",
		["nil"],
		["cons", ["hd", "Term"], ["tl", "TList"]],
	],
]


clog = open("solver.txt", "a")
def log(msg = ""):
	clog.write(msg + "\n")


class ErlangSMT(cgs.AbstractErlangSolver):

	def __init__(self):
		self.constants = []
		self.assertions = []
		self.solver = None
		self.model = None
		self.reset_solver()

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
		self.solver = smt.SolverZ3()

	def add_axioms(self):
		"""
		Adds the axioms from memory to the solver.
		"""
		pass

	def solve(self):
		"""
		Solves a constraint set and returns the result.
		"""
		log("***")
		log(smt.encode(["declare-datatypes", [], datatypes]))
		for constant in self.constants:
			log(smt.encode(["declare-const", "|{}|".format(constant), "Term"]))
		for assertion in self.assertions:
			log(smt.encode(["assert", assertion]))
		log(smt.encode(["check-sat"]))
		tpl = self.solver.solve(datatypes, self.constants, self.assertions)
		self.model = tpl[1]
		log(tpl[0])
		if self.model is not None:
			log(smt.encode(["get-value", ["|{}|".format(constant) for constant in self.constants]]))
			log(smt.encode(self.model))
		return tpl[0]

	def encode_model(self):
		"""
		Encodes the resulting model to JSON.
		"""
		return [({"s": item[0][1:-1]}, self.encode(item[1])) for item in self.model]

	# =========================================================================
	# Private Methods.
	# =========================================================================

	def decode(self, data):
		"""
		Decodes a JSON term to its SMT representation
		"""
		if "s" in data:
			if data["s"] not in self.constants:
				self.constants.append(data["s"])
			return "|{}|".format(data["s"])
		elif data["t"] == cc.JSON_TYPE_ANY:
			return ["bool", str(data["v"]).lower()] # TODO boolean
		elif data["t"] == cc.JSON_TYPE_INT:
			return ["int", str(data["v"])]
		elif data["t"] == cc.JSON_TYPE_LIST:
			return ["list", "nil"] # TODO list
		else:
			log(str(data))
			return None # TODO decode term

	def encode(self, data):
		if data[0] == "bool":
			return {"t": cc.JSON_TYPE_ANY, "v": data[1] == "true"}
		elif data[0] == "int":
			return {"t": cc.JSON_TYPE_INT, "v": int(data[1])}
		elif data[0] == "list":
			return {"t": cc.JSON_TYPE_LIST, "v": []} # TODO list
		return None # TODO encode term

	# -------------------------------------------------------------------------
	# Parse internal commands.
	# -------------------------------------------------------------------------

	def mfa_params(self, *args):
		"""
		Stores the entry point MFA's symbolic parameters.
		"""
		self.constants = [x["s"] for x in args]

	def mfa_spec(self, *spec):
		"""
		Stores the spec of the entry point MFA.
		"""
		pass

	# -------------------------------------------------------------------------
	# Constraints.
	# -------------------------------------------------------------------------

	def guard_true(self, term):
		"""
		Asserts the predicate: term == true
		"""
		t = self.decode(term)
		self.assertions.append(["=", t, ["bool", "true"]])

	def guard_false(self, term):
		"""
		Asserts the predicate: term == false
		"""
		t = self.decode(term)
		self.assertions.append(["=", t, ["bool", "false"]])

	def match_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 == term2
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertions.append(["=", t1, t2])

	def match_not_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 != term2
		"""
		t1 = self.decode(term1);
		t2 = self.decode(term2);
		self.assertions.append(["not", ["=", t1, t2]])

	def list_empty(self, term):
		"""
		Asserts that: term is an empty list.
		"""
		t = self.decode(term)
		self.assertions.append(["=", t, ["list", "nil"]])

	def list_not_lst(self, term):
		"""
		Asserts that: term is not list.
		"""
		t = self.decode(term)
		self.assertions.append(["not", ["is-list", t]])

	# -------------------------------------------------------------------------
	# Reversed constraints.
	# -------------------------------------------------------------------------

	def guard_true_reversed(self, term):
		"""
		Asserts the predicate: Not (term == true)
		"""
		self.guard_false(term)

	def guard_false_reversed(self, term):
		"""
		Asserts the predicate: Not (term == false)
		"""
		self.guard_true(term)

	def match_equal_reversed(self, term1, term2):
		"""
		Asserts the predicate: Not (term1 == term2)
		"""
		self.match_not_equal(term1, term2)

	def match_not_equal_reversed(self, term1, term2):
		"""
		Asserts the predicate: Not (term1 != term2)
		"""
		self.match_equal(term1, term2)

	def list_not_lst_reversed(self, term):
		"""
		Asserts that: Not (term is not list).
		"""
		t = self.decode(term)
		self.assertions.append(["is-list", t])

	# -------------------------------------------------------------------------
	# Erlang BIFs or MFAs treated as BIFs.
	# -------------------------------------------------------------------------

	def is_integer(self, term1, term2):
		"""
		Asserts that: term1 == is_integer(term2).
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.assertions.append(["=", t1, ["bool", ["is-int", t2]]])
