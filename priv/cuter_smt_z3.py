from cuter_smt import Solver_SMT
import cuter_smt_process


class Solver_SMT_Z3(Solver_SMT):

	name = "Z3"

	# =========================================================================
	# Public API.
	# =========================================================================

	def start_process(self):
		timeout = cuter_smt_process.timeout
		arguments = ["z3", "-smt2", "-T:{}".format(timeout), "-in"]
		self.process = cuter_smt_process.Solver(arguments)

	def check_process(self):
		arguments = ["z3", "-version"]
		return cuter_smt_process.check(arguments)

	# -------------------------------------------------------------------------
	# Constraints.
	# -------------------------------------------------------------------------

	def erl_lambda(self, *args):
		"""
		Assert that a lambda application has succeeded.
		"""
		ret = self.decode(args[0])
		fun = self.decode(args[1])
		tlist = "tn"
		tlist_length = 0
		for arg in reversed(args[2:]):
			tlist = ["tc", self.decode(arg), tlist]
			tlist_length += 1
		if hasattr(self, "flist_depth"):
			self.flist_depth += 1
		else:
			self.flist_depth = 0
		self.commands.append(["assert", [
			"and",
			["is-fun", fun],
			["=", ["fa", ["fv", fun]], self.build_int(tlist_length)],
			self.FListEquals(["fm", ["fv", fun]], tlist, ret, self.build_int(self.flist_depth)),
		]])

	# -------------------------------------------------------------------------
	# SMTLIB recursive functions
	# -------------------------------------------------------------------------

	def FListEquals(self, f, x, y, d):
		"""
		flist-equals return whether f(x) = y with depth at most d
		"""
		if self.append_to_library("flist-equals"):
			self.append_to_library("define-fun-rec")
			self.commands.append(["define-fun-rec", "flist-equals", [["f", "FList"], ["x", "TList"], ["y", "Term"], ["d", "Int"]], "Bool", [
				"or",
				["and", [">=", "d", "0"], ["is-fc", "f"], ["=", ["fx", "f"], "x"], ["=", ["fy", "f"], "y"]],
				["and", [">", "d", "0"], ["is-fc", "f"], ["not", ["=", ["fx", "f"], "x"]], ["flist-equals", ["ft", "f"], "x", "y", ["-", "d", "1"]]],
			]])
		return ["flist-equals", f, x, y, d]
