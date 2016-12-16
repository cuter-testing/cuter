import subprocess

debug = False

if debug:
	import os.path
	filename = "solver.txt"
	if os.path.isfile(filename):
		clog = open(filename, "a")
	else:
		clog = open(filename, "w")
def log(msg = ""):
	if debug:
		clog.write(msg + "\n")


def encode(obj):
	"""
	Encodes a structured list to an SMTLIB string
	"""
	if isinstance(obj, list):
		return "(" + " ".join(map(encode, obj)) + ")"
	else:
		return obj


def decode(smt):
	"""
	Decodes an SMTLIB string to a structured list
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

	def __init__(self, arguments):
		"""
		Creates a subprocess using provided program arguments
		"""
		self.process = subprocess.Popen(
			arguments,
			stdin=subprocess.PIPE,
			stdout=subprocess.PIPE,
			stderr=subprocess.PIPE,
			universal_newlines=True
		)
		log(";")

	def write(self, stmt):
		line = encode(stmt)
		self.process.stdin.write(line + "\n")
		log(line)

	def read(self):
		open_cnt = 0
		close_cnt = 0
		lines = []
		while True:
			line = self.process.stdout.readline()[:-1]
			lines.append(line)
			open_cnt += line.count("(")
			close_cnt += line.count(")")
			if open_cnt == close_cnt:
				break;
		smt = "\n".join(lines)
		log(smt)
		if open_cnt == 0 and close_cnt == 0:
			return smt
		return decode(smt)


class SolverCVC4(Solver):

	def __init__(self):
		Solver.__init__(self, ["cvc4", "--lang", "smt", "--produce-models"])

	@staticmethod
	def fix_names(obj): # TODO fix_names
		return [["|{}|".format(item[0]), item[1]] for item in obj]


class SolverZ3(Solver):

	def __init__(self):
		Solver.__init__(self, ["z3", "-smt2", "-in"])
