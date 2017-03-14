# =================
# struct -> smtlib: serialize
# smtlib -> struct: unserialize
# -----------------
# struct -> python: parse
# python -> struct: build
# -----------------
# struct -> erlang: encode
# erlang -> struct: decode
# =================


# ------------------------
# convert from & to smtlib
# ------------------------

def serialize(expr):
	"""
	Serialize a structured list to an SMTLIB string.
	"""
	if isinstance(expr, list):
		return "(" + " ".join(map(serialize, expr)) + ")"
	else:
		return expr

def unserialize(smt, cur = None):
	"""
	Unserialize an SMTLIB string to a structured list.
	"""
	if cur is None:
		return unserialize(smt, 0)[0]
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
			node = unserialize(smt, cur)
			nodes.append(node[0])
			cur = node[2]
		end = cur + 1
		return (nodes, beg, end)
	else:
		beg = cur
		if smt[cur] == "\"":
			cur += 1
			while True:
				if smt[cur] == "\"":
					cur += 2
					if smt[cur] != "\"":
						break;
				else:
					cur += 1
		else:
			while smt[cur] != ")" and not smt[cur].isspace():
				cur += 1
		end = cur
		return (smt[beg:end], beg, end)

# ------------------------
# convert from & to python
# ------------------------

def parse_bool(expr):
	if expr == "true" or expr == "false":
		return expr == "true"
	assert False, "parse_bool({})".format(str(expr))

def parse_int(expr):
	if isinstance(expr, list):
		if expr[0] == "-" and len(expr) == 2:
			return -parse_int(expr[1])
	else:
		return int(expr)
	assert False, "parse_int({})".format(str(expr))

def parse_real(expr):
	if isinstance(expr, list):
		if expr[0] == "-" and len(expr) == 2:
			return -parse_real(expr[1])
		if expr[0] == "/" and len(expr) == 3:
			return parse_real(expr[1]) / parse_real(expr[2])
	else:
		return float(expr)
	clg.debug_info()
	assert False, "parse_real({})".format(str(expr))

def expand_lets(expr, lets = {}):
	if not isinstance(expr, list):
		if expr in lets:
			return lets[expr]
		else:
			return expr
	elif not expr:
		return []
	elif expr[0] == "let":
		assert len(expr) == 3, "expand_lets({})".format(str(expr))
		lets_copy = lets.copy()
		for var in expr[1]:
			assert len(var) == 2 and not isinstance(var[0], list), "expand_lets({})".format(str(expr))
			lets_copy[var[0]] = expand_lets(var[1], lets)
		return expand_lets(expr[2], lets_copy)
	else:
		return [expand_lets(item, lets) for item in expr]

def build_bool(value):
	return "true" if value else "false"

def build_int(num):
	if num < 0:
		return ["-", str(-num)]
	else:
		return str(num)

def build_real(num):
	if num < 0:
		return ["-", str(-num)]
	else:
		return str(num)

def build_tlist(items):
	tlist = "tn"
	for item in reversed(items):
		tlist = ["tc", item, tlist]
	return tlist

def build_ilist(items):
	ilist = "in"
	for item in reversed(items):
		ilist = ["ic", build_int(item), ilist]
	return ilist

def build_slist(items):
	slist = "sn"
	for item in reversed(items):
		slist = ["sc", "true" if item else "false", slist]
	return slist


# ------------------------
# convert from & to erlang
# ------------------------

# encode and decode are implemented in 'Private Methods' section of class ErlangSMT


# ----------------
# useful constants
# ----------------

false = ["atom", build_ilist(map(ord, "false"))]

true = ["atom", build_ilist(map(ord, "true"))]

# --------------------
# simple SMTLIB macros
# --------------------

def And(*expr):
	ret = ["and"]
	ret.extend(expr)
	return ret

def Or(*expr):
	ret = ["or"]
	ret.extend(expr)
	return ret

def Ite(cond, expr_then, expr_else):
	return ["ite", cond, expr_then, expr_else]

def IsBool(expr):
	return [
		"or",
		["=", expr, true],
		["=", expr, false],
	]

def IsInt(expr):
	return ["is-int", expr]

def IsReal(expr):
	return ["is-real", expr]

def IsNum(expr):
	return Or(IsInt(expr), IsReal(expr))

def BoolToAtom(expr):
	return Ite(expr, true, false)

def AtomToBool(expr):
	return ["=", expr, true]

def AtomToInt(expr):
	return Ite(AtomToBool(expr), build_int(1), build_int(0))

def NumBinOp(operator, t0, t1, t2):
	"""
	Return whether t0 == t1 <op> t2.
	"""
	return [
		"and",
		IsNum(t1),
		IsNum(t2),
		[
			"ite",
			And(IsInt(t1), IsInt(t2)),
			["=", t0, ["int", [operator, ["iv", t1], ["iv", t2]]]],
			[
				"=",
				t0,
				[
					"real",
					[
						operator,
						["ite", IsInt(t1), ["to_real", ["iv", t1]], ["rv", t1]],
						["ite", IsInt(t2), ["to_real", ["iv", t2]], ["rv", t2]]
					]
				]
			]
		],
	]
