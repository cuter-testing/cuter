# -*- coding: utf-8 -*-

import cuter_generic_solver as cgs
import cuter_common as cc
import cuter_logger as clg

clog = open("output.smt", "w")
def log(msg = ""):
	clog.write(msg + "\n")

def smt(obj):
	if type(obj) is str:
		return obj
	else:
		return "(" + " ".join(map(smt, obj)) + ")"

class ErlangSMT(cgs.AbstractErlangSolver):
	def __init__(self):
		self.model = None
		self.params = []
		self.axioms = []
		self.symbols = []

	# =========================================================================
	# Public API.
	# =========================================================================

	def fix_parameter(self, p, v):
		"""
		Fixes a symbolic variable to a specific value.
		"""
		pass

	def add_axioms(self):
		"""
		Adds the axioms from memory to the solver.
		"""
		dd = [
			"declare-datatypes", [],
			[
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
			],
		]
		log(smt(dd))
		for symbol in self.symbols:
			log(smt(["declare-const", "|{}|".format(symbol), "Term"]))
		for axiom in self.axioms:
			log(axiom)
		log(smt(["check-sat"]))
		log(smt(["get-model"]))

	def solve(self):
		"""
		Solves a constraint set and returns the result.
		"""
		return cc.SOLVER_STATUS_SAT

	def reset_solver(self):
		"""
		Resets the solver.
		"""
		pass

	def encode_model(self):
		"""
		Encodes the resulting model to JSON.
		"""
		return [({"s": p}, {"t": cc.JSON_TYPE_INT, "v": 1}) for p in self.params]

	# =========================================================================
	# Private Methods.
	# =========================================================================

	def decode(self, data):
		"""
		Decodes a JSON term to its SMT representation
		"""
		if "s" in data:
			if data["s"] not in self.symbols:
				self.symbols.append(data["s"])
			return "|{}|".format(data["s"])
		if data["t"] == cc.JSON_TYPE_INT:
			return "(int {})".format(data["v"])
		else:
			return None # TODO decode term

	# -------------------------------------------------------------------------
	# Parse internal commands.
	# -------------------------------------------------------------------------

	def mfa_params(self, *args):
		"""
		Stores the entry point MFA's symbolic parameters.
		"""
		self.params = [x["s"] for x in args]

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
		self.axioms.append("(assert (= {} (bool true)))".format(t))

	def guard_false(self, term):
		"""
		Asserts the predicate: term == false
		"""
		t = self.decode(term)
		self.axioms.append("(assert (= {} (bool false)))".format(t))

	def match_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 == term2
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append("(assert (= {} {}))".format(t1, t2))

	def match_not_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 != term2
		"""
		t1 = self.decode(term1);
		t2 = self.decode(term2);
		self.axioms.append("(assert (not (= {} {})))".format(t1, t2))

	def list_not_lst(self, term):
		"""
		Asserts that: term is not list.
		"""
		t = self.decode(term)
		self.axioms.append("(assert (not (is-list {})))".format(t))

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
		self.axioms.append("(assert (is-list {}))".format(t))

	# -------------------------------------------------------------------------
	# Erlang BIFs or MFAs treated as BIFs.
	# -------------------------------------------------------------------------

	def is_integer(self, term1, term2):
		"""
		Asserts that: term1 == is_integer(term2).
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append("(assert (= {} (bool (is-int {}))))".format(t1, t2))
