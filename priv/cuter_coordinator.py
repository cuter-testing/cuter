import cuter_common as cc
import cuter_logger as clg
from cuter_proto_log_entry_pb2 import LogEntry
from cuter_smt_z3 import Solver_SMT_Z3
from cuter_smt_cvc4 import Solver_SMT_CVC4

from multiprocessing import Process, Queue, Pipe


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
			if arg is None:
				pass #clg.debug_info("initial call")
			else:
				pass #clg.debug_info("fix parameter\n{}@\n{}".format(arg[0], arg[1]))
			for solver in self.solvers:
				solver.reset()
				if arg is not None:
					solver.fix_parameter(arg[0], arg[1])
				status = solver.solve()
				#clg.debug_info("solver {} sent\n{}".format(solver.name, status))
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


def solver_handler(i, solver, queue, pipe):
	while True:
		command = pipe.recv()
		if command == "reset":
			solver.reset()
		elif command == "fix":
			arg = pipe.recv()
			solver.fix_parameter(arg[0], arg[1])
		elif command == "solve":
			status = solver.solve()
			if cc.is_sat(status):
				model = solver.get_model()
				queue.put((i, status, model,))
			else:
				queue.put((i, status,))
		else:
			break
	queue.close()
	pipe.close()


class Solver_Coordinator_Race(Solver_Coordinator):
	"""
	fire all solvers and accept the first sat/unsat answer
	"""

	def main_init(self):
		self.solvers = [self.solver_z3, self.solver_cvc4]

	def main_solve(self):
		queue = Queue()
		ps = []
		n = len(self.solvers)
		clg.debug_info("firing {} solvers:".format(n))
		for i in range(n):
			solver = self.solvers[i]
			clg.debug_info("#{}: {}".format(i, solver.name))
			conn_read, conn_write = Pipe(False)
			# TODO do not duplicate solver instance
			proc = Process(target=solver_handler, args=(i, solver, queue, conn_read,))
			ps.append((proc, conn_write,))
			proc.start()
		solved = False
		for arg in [None] + self.mapping:
			if arg is None:
				clg.debug_info("initial call")
			else:
				clg.debug_info("fix parameter\n{}@\n{}".format(arg[0], arg[1]))
			for p in ps:
				p[1].send("reset")
				if arg is not None:
					p[1].send("fix")
					p[1].send(arg)
				p[1].send("solve")
			for j in range(n):
				# TODO some solver processes maybe still running
				data = queue.get()
				i, status = data[0:2]
				clg.debug_info("solver #{} sent\n{}".format(i, status))
				if cc.is_sat(status):
					self.model = data[2]
					solved = True
					break
				elif cc.is_unsat(status):
					solved = True
					break
			if solved:
				break
		queue.close()
		for p in ps:
			p[1].send("exit")
			p[1].close()
			p[0].terminate()
			p[0].join()
		return status
