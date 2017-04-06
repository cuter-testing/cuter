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
