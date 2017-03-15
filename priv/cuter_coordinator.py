import cuter_common as cc
from cuter_smt_z3 import Solver_SMT_Z3
from cuter_smt_cvc4 import Solver_SMT_CVC4


# TODO start_process in cases like z3py
# TODO design pattern


class Solver_Coordinator:

	def __init__(self):
		self.solvers = []
		self.solver_z3 = Solver_SMT_Z3()
		assert self.solver_z3.check_process(), "z3 is not installed"
		self.solvers.append(self.solver_z3)
		self.solver_cvc4 = Solver_SMT_CVC4()
		if not self.solver_cvc4.check_process():
			self.solver_cvc4 = None
		else:
			self.solvers.append(self.solver_cvc4)
		self.model = None

	def command(self, entry, rev):
		for solver in self.solvers:
			solver.command_toSolver(entry, rev)

	def solve(self):
		if self.solver_cvc4 is None:
			return self.strategy_z3()
		return self.strategy_priority()

	def get_model(self):
		return self.model

	def strategy_z3(self):
		"""
		use only Z3
		"""
		solver = self.solver_z3
		solver.start_process()
		solver.add_axioms()
		status = solver.solve()
		if cc.is_sat(status):
			self.model = solver.encode_model()
		# TODO z3 supports incremental solving; so we could fix a variable and repeat
		return status

	def strategy_cvc4(self):
		"""
		use only CVC4
		"""
		solver = self.solver_cvc4
		solver.start_process()
		solver.add_axioms()
		status = solver.solve()
		if cc.is_sat(status):
			self.model = solver.encode_model()
		# TODO cvc4 supports incremental solving if --incremental flag is on; so we could fix a variable and repeat
		return status

	def strategy_priority(self):
		"""
		try Z3; if timeout or unknown, try CVC4
		"""
		for solver in self.solvers:
			solver.start_process()
			solver.add_axioms()
			status = solver.solve()
			if cc.is_sat(status):
				self.model = solver.encode_model()
				break
			elif cc.is_unsat(status):
				break
			# TODO incremental solving strategy in case of timeout or unknown status
		return status

	def strategy_guess(self):
		"""
		select a solver according to the present constraint set
		"""
		assert False # TODO implement
