import cuter_smt
import cuter_smt_process


class Solver_SMT_CVC4(cuter_smt.Solver_SMT):

	name = "CVC4"

	datatypes = [
		[
			"Term",
			["int", ["iv", "Int"]],
			["real", ["rv", "Real"]],
			["list", ["lv", "TList"]],
			["tuple", ["tv", "TList"]],
			["atom", ["av", "IList"]],
			["str", ["sv", "SList"]],
			["fun", ["fv", "Int"]],
		],
		[
			"TList",
			["tn"],
			["tc", ["th", "Term"], ["tt", "TList"]],
		],
		[
			"IList",
			["in"],
			["ic", ["ih", "Int"], ["it", "IList"]],
		],
		[
			"SList",
			["sn"],
			["sc", ["sh", "Int"], ["st", "SList"]],
		],
		[
			"FList",
			["fn"],
			["fc", ["fx", "TList"], ["fy", "Term"], ["ft", "FList"]],
		],
	]

	# =========================================================================
	# Public API.
	# =========================================================================

	def start_process(self):
		timeout = cuter_smt_process.timeout
		arguments = ["cvc4", "--lang=smt2.5", "--tlimit={}".format(timeout * 1000)]
		if "define-fun-rec" in self.library:
			arguments.append("--fmf-fun")
		# TODO "--incremental" - if enabled, solver returns unknown while testing collection:f/1
		# TODO "--rewrite-divk" - still solver outputs an error while testing collection:k2/3
		self.process = cuter_smt_process.Solver(arguments)
		self.process.write(["set-logic", "UFDTLIRA"])

	def check_process(self):
		arguments = ["cvc4", "--version"]
		return cuter_smt_process.check(arguments)

	# =========================================================================
	# Private Methods.
	# =========================================================================

	# ------------------------
	# convert from & to python
	# ------------------------

	def build_slist(self, items):
		slist = "sn"
		for item in reversed(items):
			slist = ["sc", self.build_int(1 if item else 0), slist]
		return slist

	# ------------------------
	# convert from & to erlang
	# ------------------------

	def encode_str(self, node):
		ret = []
		while node != "sn":
			assert isinstance(node, list) and len(node) == 3 and node[0] == "sc"
			ret.append(self.parse_int(node[1]) != 0)
			node = node[2]
		return ret

	# -------------------------------------------------------------------------
	# Parse internal commands.
	# -------------------------------------------------------------------------

	def int2bv(self, n, b):
		assert isinstance(b, int) and b >= 0, "b must be a non-negative integer"
		assert isinstance(n, int) and n >= 0, "n must be a non-negative integer"
		ret = []
		while b > 0:
			ret.append(self.build_int(n % 2))
			n /= 2
			b -= 1
		#assert n == 0, "n overflows b bits as an unsigned integer" # TODO erlang sends <<42:0>> in bitstr:f22/2
		ret.reverse()
		return ret

	def var2bv(self, n, b):
		assert isinstance(b, int) and b >= 0, "b must be a non-negative integer"
		conj = [
			"and",
			["is-int", n],
		]
		n = ["iv", n]
		conj.append(["<=", "0", n])
		ret = []
		while b > 0:
			ret.append(["mod", n, "2"])
			n = ["div", n, "2"]
			b -= 1
		conj.append(["=", n, "0"])
		ret.reverse()
		self.commands.append(["assert", conj])
		return ret

	def concat_segs(self, *terms):
		"""
		Concatenate many bitstrings into a large binary.
		"""
		t = self.decode(terms[0])
		r = self.decode(terms[1])
		v = ["sv", r]
		conj = [
			"and",
			["is-str", t],
			["is-str", r],
		]
		for term in reversed(terms[2:]):
			b = self.decode(term)
			conj.append(self.IsBool(b))
			v = ["sc", self.AtomToInt(b), v]
		conj.append(["=", ["sv", t], v])
		self.commands.append(["assert", conj])

	def nonempty_bitstr(self, term1, term2, term):
		"""
		Assert that: term is a nonempty bitstring.
		"""
		t = self.decode(term)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.commands.append(["assert", [
			"and",
			["is-str", t],
			["is-sc", ["sv", t]],
			self.IsBool(t1),
			["=", self.AtomToInt(t1), ["sh", ["sv", t]]],
			["is-str", t2],
			["=", ["sv", t2], ["st", ["sv", t]]],
		]])
