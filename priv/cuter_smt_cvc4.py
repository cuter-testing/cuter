from cuter_smt import Solver_SMT
import cuter_smt_process


class Solver_SMT_CVC4(Solver_SMT):

	name = "CVC4"

	# =========================================================================
	# Public API.
	# =========================================================================

	def start_process(self):
		timeout = cuter_smt_process.timeout
		arguments = ["cvc4", "--lang=smt2.5", "--tlimit={}".format(timeout * 1000)]
		if "define-fun-rec" in self.library:
			arguments.append("--fmf-fun")
		self.process = cuter_smt_process.Solver(arguments)
		self.process.write(["set-logic", "UFDTLIRA"])

	def check_process(self):
		arguments = ["cvc4", "--version"]
		return cuter_smt_process.check(arguments)
