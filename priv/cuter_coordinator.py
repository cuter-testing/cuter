import cuter_common as cc
from cuter_smt_z3 import Solver_SMT_Z3
from cuter_smt_cvc4 import Solver_SMT_CVC4


class Solver_Coordinator:

	def __init__(self):
		# TODO what happens when an optional solver is not installed?
		self.solvers = [
			Solver_SMT_Z3(),
			Solver_SMT_CVC4(),
		]
		self.model = None

	def command(self, entry, rev):
		for solver in self.solvers:
			solver.command_toSolver(entry, rev)

	def solve(self):
		for solver in self.solvers:
			# TODO start_process in cases like z3py
			solver.start_process()
			solver.add_axioms()
			status = solver.solve()
			if cc.is_sat(status):
				self.model = solver.encode_model()
				break
			elif cc.is_unsat(status):
				break
			# TODO handling timeout and unknown status strategy
		return status

	def get_model(self):
		return self.model
