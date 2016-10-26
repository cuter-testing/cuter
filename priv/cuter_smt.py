# -*- coding: utf-8 -*-

import cuter_generic_solver as cgs
import cuter_common as cc
import cuter_logger as clg

import subprocess

clog = open("output.log", "w")
def log(msg = ""):
	clog.write(msg + "\n")

def smt(obj):
	if type(obj) is str:
		return obj
	else:
		return "(" + " ".join(map(smt, obj)) + ")"


class Solver:
	def __init__(self, args):
		self.process = subprocess.Popen(
			args,
			stdin=subprocess.PIPE,
			stdout=subprocess.PIPE,
			stderr=subprocess.PIPE,
			universal_newlines=True
		)
		self.status = None

	def write(self, cmd):
		self.process.stdin.write(cmd + "\n")

	def load(self, symbols, axioms):
		self.symbols = symbols
		self.axioms = axioms
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
		self.write(smt(dd))
		for symbol in self.symbols:
			self.write(smt(["declare-const", "|{}|".format(symbol), "Term"]))
		for axiom in self.axioms:
			self.write(smt(axiom))

	def check_sat(self):
		if self.status is None:
			self.write(smt(["check-sat"]))
			self.status = self.process.stdout.readline()[:-1]
			if self.status == cc.SOLVER_STATUS_SAT:
				self.write(smt(["get-value", ["|{}|".format(symbol) for symbol in self.symbols]]))
				self.write(smt(["exit"]))
				self.process.stdin.close()
				self.response = self.process.stdout.read()
				self.model = self.parse_response()[0]
			else: # TODO other status responses
				self.write(smt(["exit"]) + "\n")
				self.process.stdin.close()
				self.process.stdout.read()
		return self.status

	def parse_response(self, cur = 0):
		while self.response[cur].isspace():
			cur += 1
		if self.response[cur] == "(":
			nodes = []
			beg = cur
			cur += 1
			while True:
				while self.response[cur].isspace():
					cur += 1
				if self.response[cur] == ")":
					break
				node = self.parse_response(cur)
				nodes.append(node[0])
				cur = node[2]
			end = cur + 1
			return (nodes, beg, end)
		else:
			beg = cur
			while self.response[cur] != ")" and not self.response[cur].isspace():
				cur += 1
			end = cur
			return (self.response[beg:end], beg, end)

	def get_name(self, name):
		return name[1:-1]


class SolverCVC4(Solver):
	def __init__(self):
		Solver.__init__(self, ["cvc4", "--lang", "smt", "--produce-models"])

	def get_name(self, name):
		return name


class SolverZ3(Solver):
	def __init__(self):
		Solver.__init__(self, ["z3", "-smt2", "-in"])


class ErlangSMT(cgs.AbstractErlangSolver):

	def __init__(self):
		self.params = [] # TODO what's this?
		self.axioms = []
		self.symbols = []
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
		self.solver = SolverZ3()

	def add_axioms(self):
		"""
		Adds the axioms from memory to the solver.
		"""
		self.solver.load(self.symbols, self.axioms)

	def solve(self):
		"""
		Solves a constraint set and returns the result.
		"""
		return self.solver.check_sat()

	def encode_model(self):
		"""
		Encodes the resulting model to JSON.
		"""
		for symbol in self.solver.model:
			log("{} {}".format(self.solver.get_name(symbol[0]), str(symbol[1])))
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
			return ["int", str(data["v"])]
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
		self.axioms.append(["assert", ["=", t, ["bool", "true"]]])

	def guard_false(self, term):
		"""
		Asserts the predicate: term == false
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["=", t, ["bool", "false"]]])

	def match_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 == term2
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["=", t1, t2]])

	def match_not_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 != term2
		"""
		t1 = self.decode(term1);
		t2 = self.decode(term2);
		self.axioms.append(["assert", ["not", ["=", t1, t2]]])

	def list_not_lst(self, term):
		"""
		Asserts that: term is not list.
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["not", ["is-list", t]]])

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
		self.axioms.append(["assert", ["is-list", t]])

	# -------------------------------------------------------------------------
	# Erlang BIFs or MFAs treated as BIFs.
	# -------------------------------------------------------------------------

	def is_integer(self, term1, term2):
		"""
		Asserts that: term1 == is_integer(term2).
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["=", t1, ["bool", ["is-int", t2]]]])
