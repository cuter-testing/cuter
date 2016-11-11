import subprocess
import os.path

filename = "solver.txt"
if os.path.isfile(filename):
	clog = open(filename, "a")
else:
	clog = open(filename, "w")
def log(msg = ""):
	clog.write(msg + "\n")


def encode(obj):
	"""
	Encodes a structured list to an SMT string
	"""
	if isinstance(obj, list):
		return "(" + " ".join(map(encode, obj)) + ")"
	else:
		return obj


def decode(smt):
	"""
	Decodes an SMT string to a structured list
	"""
	return decode_aux(smt, 0)[0]


def decode_aux(smt, cur):
	while smt[cur].isspace():
		cur += 1
	if smt[cur] == "(":
		nodes = []
		beg = cur
		cur += 1
		while True:
			while smt[cur].isspace():
				cur += 1
			if smt[cur] == ")":
				break
			node = decode_aux(smt, cur)
			nodes.append(node[0])
			cur = node[2]
		end = cur + 1
		return (nodes, beg, end)
	else:
		beg = cur
		while smt[cur] != ")" and not smt[cur].isspace():
			cur += 1
		end = cur
		return (smt[beg:end], beg, end)


class Solver:

	def __init__(self, args):
		self.args = args

	def solve(self, stmts, vars):
		process = subprocess.Popen(
			self.args,
			stdin=subprocess.PIPE,
			stdout=subprocess.PIPE,
			stderr=subprocess.PIPE,
			universal_newlines=True
		)
		log(";")
		for stmt in stmts:
			process.stdin.write(encode(stmt) + "\n")
			log(encode(stmt))
		process.stdin.write(encode(["check-sat"]) + "\n")
		log(encode(["check-sat"]))
		status = process.stdout.readline()[:-1]
		log(status)
		if status == "sat":
			process.stdin.write(encode(["get-value", vars]) + "\n")
			log(encode(["get-value", vars]))
			process.stdin.write(encode(["exit"]) + "\n")
			process.stdin.close()
			smt = process.stdout.read()
			obj = self.fix_names(decode(smt))
			log(smt)
		else:
			process.stdin.write(encode(["exit"]) + "\n")
			process.stdin.close()
			process.stdout.read()
			obj = None
		return (status, obj)

	@staticmethod
	def fix_names(obj):
		return obj


class SolverCVC4(Solver):

	def __init__(self):
		Solver.__init__(self, ["cvc4", "--lang", "smt", "--produce-models"])

	@staticmethod
	def fix_names(obj):
		return [["|{}|".format(item[0]), item[1]] for item in obj]


class SolverZ3(Solver):

	def __init__(self):
		Solver.__init__(self, ["z3", "-smt2", "-in"])
