import cuter_common as cc
import cuter_logger as clg
from cuter_proto_log_entry_pb2 import LogEntry
from cuter_smt_z3 import Solver_SMT_Z3
from cuter_smt_cvc4 import Solver_SMT_CVC4


# TODO start_process etc in non-smt solvers, like z3py


class Solver_Coordinator:
	"""
	default; use only Z3
	"""

	def __init__(self):
		self.solver_z3 = Solver_SMT_Z3()
		assert self.solver_z3.check_process(), "Z3 is not installed"
		self.solver_cvc4 = Solver_SMT_CVC4()
		if not self.solver_cvc4.check_process():
			self.main_init_z3()
		else:
			self.main_init()
		self.model = None

	def command(self, entry, rev):
		self.prev_command(entry, rev)
		if entry.type == LogEntry.OP_PARAMS:
			n = len(entry.arguments) / 2
			self.mapping = []
			for i in range(n):
				self.mapping.append((entry.arguments[i], entry.arguments[n + i]))
			# TODO do not pass second half of entry.arguments
		for solver in self.solvers:
			solver.command_toSolver(entry, rev)

	def solve(self):
		if self.solver_cvc4 is None:
			return self.main_solve_z3()
		else:
			self.prev_solve()
			return self.main_solve()

	def get_model(self):
		return self.model

	def main_init_z3(self):
		self.solver_cvc4 = None
		self.solvers = [self.solver_z3]

	def main_solve_z3(self):
		for arg in [None] + self.mapping:
			for solver in self.solvers:
				solver.reset()
				if arg is not None:
					solver.fix_parameter(arg[0], arg[1])
				status = solver.solve()
				if arg is not None:
					clg.debug_info("{}\nfix parameter\n{}@\n{}\n{}".format(solver.__class__.__name__, arg[0], arg[1], status))
				else:
					#clg.debug_info("{}\ninitial call\n{}".format(solver.__class__.__name__, status))
					pass
				if cc.is_sat(status):
					self.model = solver.get_model()
					return status
				elif cc.is_unsat(status):
					return status
		return status

	def main_init(self):
		self.main_init_z3()

	def prev_command(self, entry, rev):
		pass

	def prev_solve(self):
		pass

	def main_solve(self):
		return self.main_solve_z3()


class Solver_Coordinator_Z3(Solver_Coordinator):
	"""
	use only Z3
	"""

	pass


class Solver_Coordinator_CVC4(Solver_Coordinator):
	"""
	use only CVC4
	"""

	def main_init(self):
		self.solver_z3 = None
		self.solvers = [self.solver_cvc4]


class Solver_Coordinator_Priority(Solver_Coordinator):
	"""
	try Z3; if timeout or unknown, try CVC4
	"""

	def main_init(self):
		self.solvers = [self.solver_z3, self.solver_cvc4]


class Solver_Coordinator_Guess(Solver_Coordinator):
	"""
	select a solver according to the present constraint set
	"""

	def main_init(self):
		self.solvers = [self.solver_z3, self.solver_cvc4]
		self.typedefs = False

	def prev_command(self, entry, rev):
		if entry.type == LogEntry.OP_SPEC and not self.typedefs:
			typedefs = cc.get_type_defs_of_spec(entry.spec)
			if len(typedefs) > 0:
				self.typedefs = True

	def prev_solve(self):
		if self.typedefs:
			self.solvers.reverse()
