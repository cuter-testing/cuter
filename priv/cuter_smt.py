# -*- coding: utf-8 -*-

import cuter_generic_solver as cgs
import cuter_common as cc
import cuter_logger as clg
import smt


false = [102, 97, 108, 115, 101]
true = [116, 114, 117, 101]


def calculate_int(obj):
	if isinstance(obj, list):
		if obj[0] == "-" and len(obj) == 2:
			i0 = calculate_int(obj[1])
			return -i0
	else:
		return int(obj)
	clg.debug_info("calculate_int: unknown operation " + str(obj))
	assert False


def calculate_real(obj):
	if isinstance(obj, list):
		if obj[0] == "-" and len(obj) == 2:
			r0 = calculate_real(obj[1])
			return -r0
		if obj[0] == "/" and len(obj) == 3:
			r1 = calculate_real(obj[1])
			r2 = calculate_real(obj[2])
			return r1 / r2
	else:
		return float(obj)
	clg.debug_info("calculate_real: unknown operation " + str(obj))
	assert False


def expand_lets(obj, lets = {}):
	if not isinstance(obj, list):
		if obj in lets:
			return lets[obj]
		else:
			return obj
	elif not obj:
		return []
	elif obj[0] == "let":
		assert len(obj) == 3, "invalid let expression"
		lets_copy = lets.copy()
		for var in obj[1]:
			assert len(var) == 2 and not isinstance(var[0], list), "invalid let expression"
			lets_copy[var[0]] = expand_lets(var[1], lets)
		return expand_lets(obj[2], lets_copy)
	else:
		ret = []
		for item in obj:
			ret.append(expand_lets(item, lets))
		return ret


class ErlangSMT(cgs.AbstractErlangSolver):

	def __init__(self):
		self.options = []
		self.options.append(["set-option", ":timeout", "1000"])
		self.declarations = []

		self.declarations.append(["declare-datatypes", [], [
			[
				"Term",
				["bool", ["bval", "Bool"]],
				["int", ["ival", "Int"]],
				["real", ["rval", "Real"]],
				["list", ["lval", "TList"]],
				["tuple", ["tval", "TList"]],
				["atom", ["aval", "IList"]],
				["str", ["sval", "SList"]],
				["fun", ["fval", "Int"]],
			],
			[
				"TList",
				["nil"],
				["cons", ["hd", "Term"], ["tl", "TList"]],
			],
			[
				"IList",
				["inil"],
				["icons", ["ihd", "Int"], ["itl", "IList"]],
			],
			[
				"SList",
				["snil"],
				["scons", ["shd", "Bool"], ["stl", "SList"]],
			],
		]])
		self.declarations.append(["declare-fun", "fmap", ["Int"], ["Array", "TList", "Term"]])
		self.declarations.append(["declare-fun", "arity", ["Int"], "Int"])

		# slist_spec(l, m, n) returns whether len(l) >= m and (len(l) - m) % n == 0
		# slist_spec is efficient when n and m are given integer constants
		self.declarations.append(["define-fun-rec", "slist_spec_aux", [["l", "SList"], ["n", "Int"], ["r", "Int"]], "Bool", [
			"ite",
			["is-snil", "l"],
			["=", "r", "0"],
			["slist_spec_aux", ["stl", "l"], "n", ["-", ["ite", ["=", "r", "0"], "n", "r"], "1"]]
		]])
		self.declarations.append(["define-fun-rec", "slist_spec", [["l", "SList"], ["m", "Int"], ["n", "Int"]], "Bool", [
			"ite",
			["=", "m", "0"],
			[
				"ite",
				["=", "n", "0"],
				["is-snil", "l"],
				["slist_spec_aux", "l", "n", "0"]
			],
			["and", ["not", ["is-snil", "l"]], ["slist_spec", ["stl", "l"], ["-", "m", "1"], "n"],]
		]])

		self.declarations.append(["define-fun-rec", "slist_concat_aux", [["l1", "SList"], ["l1inv", "SList"], ["l2", "SList"]], "SList", [
			"ite",
			["is-snil", "l1"],
			[
				"ite",
				["is-snil", "l1inv"],
				"l2",
				["slist_concat_aux", "l1", ["stl", "l1inv"], ["scons", ["shd", "l1inv"], "l2"]],
			],
			["slist_concat_aux", ["stl", "l1"], ["scons", ["shd", "l1"], "l1inv"], "l2"]
		]])
		self.declarations.append(["define-fun", "slist_concat", [["l1", "SList"], ["l2", "SList"]], "SList", ["slist_concat_aux", "l1", "snil", "l2"]])

		self.declarations.append(["define-fun-rec", "slist_match", [["l1", "SList"], ["l2", "SList"]], "Bool", [
			"or",
			["is-snil", "l1"],
			[
				"and",
				["not", ["is-snil", "l2"]],
				["=", ["shd", "l1"], ["shd", "l2"]],
				["slist_match", ["stl", "l1"], ["stl", "l2"]],
			],
		]])

		# int-and returns whether n1 & n2 == n
		self.declarations.append(["define-fun-rec", "int-and-rec", [["n1", "Int"], ["n2", "Int"], ["n", "Int"]], "Bool", [
			"or",
			["=", "n1", "n", "0"],
			["=", "n2", "n", "0"],
			["=", "n1", "n2", "n", "-1"],
			[
				"and",
				["=", ["and", ["not", ["=", ["mod", "n1", "2"], "0"]], ["not", ["=", ["mod", "n2", "2"], "0"]]], ["not", ["=", ["mod", "n", "2"], "0"]]],
				["int-and-rec", ["div", "n1", "2"], ["div", "n2", "2"], ["div", "n", "2"]],
			],
		]])
		self.declarations.append(["define-fun", "int-and", [["n1", "Int"], ["n2", "Int"], ["n", "Int"]], "Bool", [
			"and",
			["implies", [">=", "n1", "0"], ["<=", "0", "n", "n1"]],
			["implies", [">=", "n2", "0"], ["<=", "0", "n", "n2"]],
			["implies", ["and", ["<", "n1", "0"], ["<", "n2", "0"]], ["<", ["+", "n1", "n2"], "n", "0"]],
			["implies", ["<", "n", "0"], ["and", ["<", "n1", "0"], ["<=", "n", "n1"], ["<", "n2", "0"], ["<=", "n", "n2"]]],
			["int-and-rec", "n1", "n2", "n"],
		]])

		# int-or returns whether n1 | n2 == n
		self.declarations.append(["define-fun-rec", "int-or-rec", [["n1", "Int"], ["n2", "Int"], ["n", "Int"]], "Bool", [
			"or",
			["=", "n1", "n", "-1"],
			["=", "n2", "n", "-1"],
			["=", "n1", "n2", "n", "0"],
			[
				"and",
				["=", ["or", ["not", ["=", ["mod", "n1", "2"], "0"]], ["not", ["=", ["mod", "n2", "2"], "0"]]], ["not", ["=", ["mod", "n", "2"], "0"]]],
				["int-or-rec", ["div", "n1", "2"], ["div", "n2", "2"], ["div", "n", "2"]],
			],
		]])
		self.declarations.append(["define-fun", "int-or", [["n1", "Int"], ["n2", "Int"], ["n", "Int"]], "Bool", [
			"and",
			["implies", ["<", "n1", "0"], ["and", ["<=", "n1", "n"], ["<", "n", "0"]]],
			["implies", ["<", "n2", "0"], ["and", ["<=", "n2", "n"], ["<", "n", "0"]]],
			["implies", ["and", [">=", "n1", "0"], [">=", "n2", "0"]], ["<=", "0", "n", ["+", "n1", "n2"]]],
			["implies", [">=", "n", "0"], ["and", ["<=", "0", "n1", "n"], ["<=", "0", "n2", "n"]]],
			["int-or-rec", "n1", "n2", "n"],
		]])

		# int-xor returns whether n1 ^ n2 == n
		self.declarations.append(["define-fun-rec", "int-xor", [["n1", "Int"], ["n2", "Int"], ["n", "Int"]], "Bool", [
			"or",
			["and", ["=", "n1", "0"], ["=", "n2", "n"]],
			["and", ["=", "n2", "0"], ["=", "n1", "n"]],
			["and", ["=", "n1", "n2", "-1"], ["=", "n", "0"]],
			["and", ["=", "n", "0"], ["=", "n1", "n2"]],
			[
				"and",
				["=", ["xor", ["not", ["=", ["mod", "n1", "2"], "0"]], ["not", ["=", ["mod", "n2", "2"], "0"]]], ["not", ["=", ["mod", "n", "2"], "0"]]],
				["int-xor", ["div", "n1", "2"], ["div", "n2", "2"], ["div", "n", "2"]],
			],
		]])

		# real-pow returns whether b ** e == p
		# real-pow isn't efficient when having to calculate the e-root of p or a large e-power of b.
		self.declarations.append(["define-fun-rec", "real-pow", [["p", "Real"], ["b", "Real"], ["e", "Real"]], "Bool", [
			"or",
			["and", ["=", "b", "0"], ["not", ["=", "e", "0"]], ["=", "p", "0"]],
			["and", ["=", "b", "1"], ["=", "p", "1"]],
			["and", ["=", "b", "-1"], ["=", ["mod", "e", "2"], "0"], ["=", "p", "1"]],
			["and", ["=", "b", "-1"], ["=", ["mod", "e", "2"], "1"], ["=", "p", "-1"]],
			["and", [">", "e", "1"], ["<", "1", "b", "p"], ["real-pow", ["/", "p", "b"], "b", ["-", "e", "1"]]],
			["and", [">", "e", "1"], ["<", "0", "p", "b", "1"], ["real-pow", ["/", "p", "b"], "b", ["-", "e", "1"]]],
			["and", [">", "e", "1"], ["=", ["mod", "e", "2"], "0"], ["<", "-1", "b", "0", "p", ["-", "b"], "1"], ["real-pow", ["/", "p", "b"], "b", ["-", "e", "1"]]],
			["and", [">", "e", "1"], ["=", ["mod", "e", "2"], "0"], ["<", "b", "-1", "1", ["-", "b"], "p"], ["real-pow", ["/", "p", "b"], "b", ["-", "e", "1"]]],
			["and", [">", "e", "1"], ["=", ["mod", "e", "2"], "1"], ["<", "-1", "b", "p", "0"], ["real-pow", ["/", "p", "b"], "b", ["-", "e", "1"]]],
			["and", [">", "e", "1"], ["=", ["mod", "e", "2"], "1"], ["<", "p", "b", "-1"], ["real-pow", ["/", "p", "b"], "b", ["-", "e", "1"]]],
			["and", ["=", "e", "1"], ["=", "b", "p"]],
			["and", ["=", "e", "0"], ["not", ["=", "b", "0"]], ["=", "p", "1"]],
			["and", ["=", "e", "-1"], ["=", ["*", "b", "p"], "1"]],
			["and", ["<", "e", "-1"], ["<", "0", "p", "1", "b"], ["real-pow", ["*", "p", "b"], "b", ["+", "e", "1"]]],
			["and", ["<", "e", "-1"], ["<", "0", "b", "1", "p"], ["real-pow", ["*", "p", "b"], "b", ["+", "e", "1"]]],
			["and", ["<", "e", "-1"], ["=", ["mod", "e", "2"], "0"], ["<", "-1", "b", "0", ["-", "b"], "1", "p"], ["real-pow", ["*", "p", "b"], "b", ["+", "e", "1"]]],
			["and", ["<", "e", "-1"], ["=", ["mod", "e", "2"], "0"], ["<", "b", "-1", "0", "p", "1", ["-", "b"]], ["real-pow", ["*", "p", "b"], "b", ["+", "e", "1"]]],
			["and", ["<", "e", "-1"], ["=", ["mod", "e", "2"], "1"], ["<", "p", "-1", "b", "0"], ["real-pow", ["*", "p", "b"], "b", ["+", "e", "1"]]],
			["and", ["<", "e", "-1"], ["=", ["mod", "e", "2"], "1"], ["<", "b", "-1", "p", "0"], ["real-pow", ["*", "p", "b"], "b", ["+", "e", "1"]]],
		]])

		self.fun_rec_cnt = 0
		self.funspec = "true"
		self.axioms = []
		self.aux_axioms = []
		self.model = None

	# =========================================================================
	# Public API.
	# =========================================================================

	def fix_parameter(self, p, v):
		"""
		Fixes a symbolic variable to a specific value.
		"""
		p = self.decode(p)
		v = self.decode(v)
		self.aux_axioms.append(["assert", ["=", p, v]])

	def reset_solver(self):
		"""
		Resets the solver.
		"""
		self.model = None

	def add_axioms(self):
		"""
		Adds the axioms from memory to the solver.
		"""
		pass

	def solve(self):
		"""
		Solves a constraint set and returns the result.
		"""
		spec = ["define-fun", "spec", [["f", "Int"], ["l", "TList"]], "Bool", self.funspec]
		solver = smt.SolverZ3()
		for cmd in self.options + self.declarations + [spec] + self.axioms + self.aux_axioms:
			solver.write(cmd)
		solver.write(["check-sat"])
		status = solver.read()
		if status == "sat":
			solver.write(["get-model"])
			model = solver.read()
			assert isinstance(model, list)
			assert len(model) > 0
			assert model[0] == "model"
			self.model = {}
			for define_fun in model[1:]:
				assert len(define_fun) == 5
				assert define_fun[0] == "define-fun"
				assert not isinstance(define_fun[1], list)
				self.model[define_fun[1]] = expand_lets(define_fun[4])
			return cc.mk_sat()
		elif status == "unsat":
			return cc.mk_unsat()
		elif status == "unknown":
			return cc.mk_unknown()
		clg.debug_info("solve: " + str(status))
		assert False

	def encode_model(self):
		"""
		Encodes the resulting model.
		"""
		entries = []
		for var in self.vars:
			if var in self.model:
				val = self.encode(self.model[var])
			else:
				val = cc.mk_any()
			entries.append(cc.mk_model_entry(cc.mk_symb(var[1:-1]), val))
		return cc.mk_model_data(cc.mk_model(entries))

	# =========================================================================
	# Private Methods.
	# =========================================================================

	def decode(self, data):
		"""
		Decodes a term to its SMT representation
		"""
		if cc.is_symb(data):
			s = "|{}|".format(cc.get_symb(data))
			if s not in self.vars and s not in self.aux_vars:
				self.aux_vars.append(s)
				self.declarations.append(["declare-const", s, "Term"])
			return s
		elif cc.is_int(data):
			return ["int", str(cc.get_int(data))]
		elif cc.is_float(data):
			return ["real", str(cc.get_float(data))]
		elif cc.is_atom(data):
			atom = cc.get_atom_chars(data)
			if atom == true:
				return ["bool", "true"]
			elif atom == false:
				return ["bool", "false"]
			else:
				return ["atom", self.value2ilist(atom)]
		elif cc.is_list(data):
			return ["list", self.value2tlist(cc.get_list_subterms(data))]
		elif cc.is_tuple(data):
			return ["tuple", self.value2tlist(cc.get_tuple_subterms(data))]
		elif cc.is_bitstring(data):
			return ["str", self.value2slist(cc.get_bits(data))]
		clg.debug_info("decoding failed: " + str(data))
		assert False

	def value2tlist(self, value):
		if not value:
			return "nil"
		else:
			return ["cons", self.decode(value[0]), self.value2tlist(value[1:])]

	def value2ilist(self, value):
		if not value:
			return "inil"
		else:
			return ["icons", str(value[0]), self.value2ilist(value[1:])]

	def value2slist(self, value):
		if not value:
			return "snil"
		else:
			return ["scons", "true" if value[0] else "false", self.value2slist(value[1:])]

	def encode(self, data, funs = []):
		if data[0] == "bool":
			if data[1] == "true":
				return cc.mk_atom(true)
			else:
				return cc.mk_atom(false)
		elif data[0] == "int":
			return cc.mk_int(calculate_int(data[1]))
		elif data[0] == "real":
			return cc.mk_float(calculate_real(data[1]))
		elif data[0] == "atom":
			node = data[1]
			v = []
			while node != "inil":
				v.append(int(node[1]))
				node = node[2]
			return cc.mk_atom(v)
		elif data[0] == "list":
			node = data[1]
			v = []
			while node != "nil":
				v.append(self.encode(node[1], funs))
				node = node[2]
			return cc.mk_list(v)
		elif data[0] == "tuple":
			node = data[1]
			v = []
			while node != "nil":
				v.append(self.encode(node[1], funs))
				node = node[2]
			return cc.mk_tuple(v)
		elif data[0] == "str":
			node = data[1]
			v = []
			while node != "snil":
				v.append(node[1] == "true")
				node = node[2]
			return cc.mk_bitstring(v)
		elif data[0] == "fun":
			fval = int(data[1])
			# if a cycle (a function calling itself recursively) is found,
			# it is obvious that the solver has selected an arbitrary term as a value
			if fval in funs:
				return cc.mk_any()
			funs = funs[:]
			funs.append(fval)
			# get return value of arity(fval)
			if "arity" in self.model:
				ite = self.model["arity"]
				while isinstance(ite, list) and len(ite) == 4 and ite[0] == "ite":
					if int(ite[1][2]) == fval:
						ite = ite[2]
					else:
						ite = ite[3]
				arity = int(ite)
				# if arity is greater than 255, we assume it is an arbitrary value selected by the solver
				# because there is no constraint limiting the function's arity; thus, we set it to zero
				if arity > 255:
					arity = 0
			else:
				arity = 0
			entries = []
			if "fmap" in self.model:
				# get return value of fmap(fval)
				ite = self.model["fmap"]
				while ite[0] in self.model:
					ite = self.model[ite[0]]
				while isinstance(ite, list) and len(ite) == 4 and ite[0] == "ite":
					if int(ite[1][2]) == fval:
						ite = ite[2]
					else:
						ite = ite[3]
				assert isinstance(ite, list) and len(ite) == 3 and ite[0] == "_" and ite[1] == "as-array"
				# return actual function
				ite = self.model[ite[2]]
				while ite[0] in self.model:
					ite = self.model[ite[0]]
				while isinstance(ite, list) and len(ite) == 4 and ite[0] == "ite":
					args = cc.get_list_subterms(self.encode(["list", ite[1][2]], funs))
					if len(args) == arity:
						value = self.encode(ite[2], funs)
						entries.append(cc.mk_fun_entry(args, value))
					ite = ite[3]
				otherwise = self.encode(ite, funs)
			else:
				otherwise = cc.mk_any()
			return cc.mk_fun(arity, entries, otherwise)
		clg.debug_info("encoding failed: " + str(data))
		assert False

	# -------------------------------------------------------------------------
	# Parse internal commands.
	# -------------------------------------------------------------------------

	def mfa_params(self, *args):
		"""
		Stores the entry point MFA's symbolic parameters.
		"""
		self.vars = []
		self.aux_vars = []
		for arg in args:
			self.decode(arg)
		self.vars = self.aux_vars
		self.aux_vars = []

	def mfa_spec(self, spec):
		"""
		Stores the spec of the entry point MFA.
		"""
		p = cc.get_spec_clauses(spec)[0]
		pms = cc.get_parameters_from_complete_funsig(p)
		for item in zip(self.vars, pms):
			self.axioms.append(["assert", self.build_spec(item[1], item[0])])

	def fun_rec(self, inner_spec):
		name = "fn" + str(self.fun_rec_cnt)
		self.fun_rec_cnt += 1
		self.declarations.append(["define-fun-rec", name, [["l", "TList"]], "Bool", [
			"or",
			["is-nil", "l"],
			[
				"and",
				["is-cons", "l"],
				inner_spec,
				[name, ["tl", "l"]],
			],
		]])
		return name

	def build_spec(self, spec, var):
		if cc.is_type_any(spec):
			return "true"
		elif cc.is_type_float(spec):
			return ["is-real", var]
		elif cc.is_type_integer(spec):
			return ["is-int", var]
		elif cc.is_type_list(spec):
			inner_spec = self.build_spec(cc.get_inner_type_from_list(spec), ["hd", "l"])
			name = self.fun_rec(inner_spec)
			return [
				"and",
				["is-list", var],
				[name, ["lval", var]],
			]
		elif cc.is_type_nonempty_list(spec):
			inner_spec = self.build_spec(cc.get_inner_type_from_nonempty_list(spec), ["hd", "l"])
			name = self.fun_rec(inner_spec)
			return [
				"and",
				["is-list", var],
				["is-cons", ["lval", var]],
				[name, ["lval", var]],
			]
		elif cc.is_type_tupledet(spec):
			inner_types = cc.get_inner_types_from_tupledet(spec)
			ret = ["and", ["is-tuple", var]]
			tlist = ["tval", var]
			for inner_type in inner_types:
				ret.append(["and", ["is-cons", tlist], self.build_spec(inner_type, ["hd", tlist])])
				tlist = ["tl", tlist]
			ret.append(["is-nil", tlist])
			return ret
		elif cc.is_type_tuple(spec):
			return ["is-tuple", var]
		elif cc.is_type_union(spec):
			ret = ["or"]
			for inner_type in cc.get_inner_types_from_union(spec):
				ret.append(self.build_spec(inner_type, var))
			return ret
		elif cc.is_type_range(spec):
			ret = ["and", ["is-int", var]]
			limits = cc.get_range_bounds_from_range(spec)
			if cc.has_lower_bound(limits):
				ret.append([">=", ["ival", var], str(cc.get_lower_bound(limits))])
			if cc.has_upper_bound(limits):
				ret.append(["<=", ["ival", var], str(cc.get_upper_bound(limits))])
			return ret
		elif cc.is_type_atom(spec):
			return ["is-atom", var]
		elif cc.is_type_bitstring(spec):
			segment_size = cc.get_segment_size_from_bitstring(spec)
			return ["and", ["is-str", var], ["slist_spec", ["sval", var], segment_size.m, segment_size.n]]
		elif cc.is_type_complete_fun(spec):
			params_spec = cc.get_parameters_from_complete_fun(spec)
			# TODO function arguments spec in list of functions etc
			# TODO if a function is to be called with wrong arguments, program must crash
			argspec = ["and"]
			tlist = "l"
			tlist_length = 0
			for param_spec in params_spec:
				argspec.append(["and", ["is-cons", "l"], self.build_spec(param_spec, ["hd", tlist])])
				tlist = ["tl", tlist]
				tlist_length += 1
			argspec.append(["is-nil", tlist])
			retspec = self.build_spec(cc.get_rettype_from_fun(spec), ["select", ["fmap", ["fval", var]], "l"])
			self.funspec = ["ite", ["=", "f", ["fval", var]], argspec, self.funspec]
			return [
				"and",
				["is-fun", var],
				["=", ["arity", ["fval", var]], str(tlist_length)],
				["forall", [["l", "TList"]], retspec],
			]
		elif cc.is_type_generic_fun(spec):
			ret_spec = cc.get_rettype_from_fun(spec)
			ret_val = ["select", ["fmap", ["fval", var]], "l"]
			return [
				"and",
				["is-fun", var],
				["forall", [["l", "TList"]], self.build_spec(ret_spec, ret_val)],
			]
		elif cc.is_type_atomlit(spec):
			return ["=", var, self.decode(cc.get_literal_from_atomlit(spec))]
		elif cc.is_type_integerlit(spec):
			literal = cc.get_literal_from_integerlit(spec)
			return ["=", var, self.decode(cc.get_literal_from_integerlit(spec))]
		clg.debug_info("unknown spec: " + str(spec))
		assert False

	def unfold_tuple(self, *terms):
		"""
		Unfolds a symbolic tuple.
		"""
		t = self.decode(terms[0])
		l = ["and"]
		l.append(["is-tuple", t])
		tlist = ["tval", t]
		for x in terms[1:]:
			s = self.decode(x)
			l.append(["is-cons", tlist])
			l.append(["=", s, ["hd", tlist]])
			tlist = ["tl", tlist]
		l.append(["is-nil", tlist])
		self.axioms.append(["assert", l])

	# TODO when constructing bitstrings, size must have a concrete non-negative integer value

	def slist_const_pair(self, n, b):
		assert isinstance(b, list) and len(b) == 2 and b[0] == "int", "b must be a concrete integer decoded term"
		b = calculate_int(b[1])
		assert b >= 0, "b must be a non-negative integer"
		assert isinstance(n, list) and len(n) == 2 and n[0] == "int", "n must be a concrete integer decoded term"
		n = calculate_int(n[1])
		assert n >= 0, "b must be a non-negative integer"
		slist = "snil"
		while b > 0:
			slist = ["scons", "true" if n % 2 != 0 else "false", slist]
			n /= 2
			b -= 1
		# assert n == 0, "n overflows b bits as an unsigned integer" # TODO cuter sends b = 0 and n = 42
		return slist

	def slist_var_pair(self, n, b):
		assert isinstance(b, list) and len(b) == 2 and b[0] == "int", "b must be a concrete integer decoded term"
		b = calculate_int(b[1])
		assert b >= 0, "b must be a non-negative integer"
		axioms = ["and"]
		axioms.append(["is-int", n])
		n = ["ival", n]
		axioms.append(["<=", "0", n])
		slist = "snil"
		while b > 0:
			slist = ["scons", ["not", ["=", ["mod", n, "2"], "0"]], slist]
			n = ["div", n, "2"]
			b -= 1
		axioms.append(["=", n, "0"])
		self.axioms.append(["assert", axioms])
		return slist

	def make_bitstr(self, symb, encodedValue, size):
		"""
		Makes a bitstring by encoding an appropriate term.
		"""
		t = self.decode(symb)
		n = self.decode(encodedValue)
		b = self.decode(size)
		slist = self.slist_var_pair(n, b)
		self.axioms.append(["assert", [
			"and",
			["is-str", t],
			["=", ["sval", t], slist],
		]])

	def concat_segs(self, *terms):
		"""
		Concatenates many bitstrings into a large binary.
		"""
		t = self.decode(terms[0])
		r = self.decode(terms[1])
		v = ["sval", r]
		l = [
			"and",
			["is-str", t],
			["is-str", r],
		]
		for term in reversed(terms[2:]):
			b = self.decode(term)
			l.append(["is-bool", b])
			v = ["scons", ["bval", b], v]
		l.append(["=", ["sval", t], v])
		self.axioms.append(["assert", l])

	def fresh_closure(self, tFun, tArity):
		"""
		Asserts that tFun is a closure with arity tArity.
		"""
		f = self.decode(tFun)
		a = self.decode(tArity)
		self.axioms.append(["assert", [
			"and",
			["is-fun", f],
			["is-int", a],
			["=", ["arity", ["fval", f]], ["ival", a]],
		]])

	def evaluated_closure(self, *args):
		"""
		Asserts that the evaluation of a closure returns some specific terms.
		"""
		self.erl_lambda(*args)

	# -------------------------------------------------------------------------
	# Constraints.
	# -------------------------------------------------------------------------

	def erl_lambda(self, *args):
		"""
		Asserts that a lambda application has succeeded.
		"""
		ret = self.decode(args[0])
		fun = self.decode(args[1])
		tlist = "nil"
		tlist_length = 0
		for arg in reversed(args[2:]):
			tlist = ["cons", self.decode(arg), tlist]
			tlist_length += 1
		self.axioms.append(["assert", [
			"and",
			["is-fun", fun],
			["=", ["arity", ["fval", fun]], str(tlist_length)],
			["spec", ["fval", fun], tlist],
			["=", ["select", ["fmap", ["fval", fun]], tlist], ret],
		]])

	def erl_lambda_reversed(self, *args): # TODO is this the opposite of erl_lambda?
		"""
		Asserts that a lambda application has failed.
		"""
		t_fun = self.decode(args[1])
		self.axioms.append(["assert", ["not", ["is-fun", t_fun]]])

	def guard_true(self, term):
		"""
		Asserts the predicate: term == true
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["=", t, ["bool", "true"]]])

	def guard_true_reversed(self, term):
		"""
		Asserts the predicate: Not (term == true)
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["not", ["=", t, ["bool", "true"]]]])

	def guard_false(self, term):
		"""
		Asserts the predicate: term == false
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["=", t, ["bool", "false"]]])

	def guard_false_reversed(self, term):
		"""
		Asserts the predicate: Not (term == false)
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["not", ["=", t, ["bool", "false"]]]])

	def match_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 == term2
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["=", t1, t2]])

	def match_equal_reversed(self, term1, term2):
		"""
		Asserts the predicate: Not (term1 == term2)
		"""
		self.match_not_equal(term1, term2)

	def match_not_equal(self, term1, term2):
		"""
		Asserts the predicate: term1 != term2
		"""
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["not", ["=", t1, t2]]])

	def match_not_equal_reversed(self, term1, term2):
		"""
		Asserts the predicate: Not (term1 != term2)
		"""
		self.match_equal(term1, term2)

	def list_nonempty(self, term):
		"""
		Asserts that: term is a nonempty list.
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["and", ["is-list", t], ["is-cons", ["lval", t]]]])

	def list_nonempty_reversed(self, term):
		"""
		Asserts that: Not(term is a nonempty list).
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["not", ["and", ["is-list", t], ["is-cons", ["lval", t]]]]])

	def list_empty(self, term):
		"""
		Asserts that: term is an empty list.
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["=", t, ["list", "nil"]]])

	def list_empty_reversed(self, term):
		"""
		Asserts that: Not(term is an empty list).
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["not", ["=", t, ["list", "nil"]]]])

	def list_not_lst(self, term):
		"""
		Asserts that: term is not list.
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["not", ["is-list", t]]])

	def list_not_lst_reversed(self, term):
		"""
		Asserts that: Not (term is not list).
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["is-list", t]])

	def tuple_sz(self, term, num):
		"""
		Asserts that: term is a tuple of size num.
		"""
		t = self.decode(term)
		n = cc.get_int(num)
		l = ["and", ["is-tuple", t]]
		c = ["tval", t]
		while n > 0:
			l.append(["is-cons", c])
			c = ["tl", c]
			n -= 1
		l.append(["is-nil", c])
		self.axioms.append(["assert", l])

	def tuple_sz_reversed(self, term, num):
		"""
		Asserts that: term is not a tuple of size num.
		"""
		self.tuple_not_sz(term, num)

	def tuple_not_sz(self, term, num):
		"""
		Asserts that: term is not a tuple of size num.
		"""
		t = self.decode(term)
		n = cc.get_int(num)
		l = ["and", ["is-tuple", t]]
		c = ["tval", t]
		while n > 0:
			l.append(["is-cons", c])
			c = ["tl", c]
			n -= 1
		l.append(["is-nil", c])
		self.axioms.append(["assert", ["not", l]])

	def tuple_not_sz_reversed(self, term, num):
		"""
		Asserts that: Not (term is not a tuple of size num).
		"""
		self.tuple_sz(term, num)

	def tuple_not_tpl(self, term, num): # TODO why is num over here?
		"""
		Asserts that: term is not a tuple.
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["not", ["is-tuple", t]]])

	def tuple_not_tpl_reversed(self, term, num): # TODO why is num over here?
		"""
		Asserts that: Not (term is not a tuple).
		"""
		t = self.decode(term)
		self.axioms.append(["assert", ["is-tuple", t]])

	def empty_bitstr(self, term):
		"""
		Asserts that: term is an empty bitstring.
		"""
		t = self.decode(term)
		self.axioms.append(["assert", [
			"and",
			["is-str", t],
			["is-snil", ["sval", t]]
		]])

	def empty_bitstr_reversed(self, term):
		"""
		Asserts that: Not (term is an empty bitstring).
		"""
		t = self.decode(term)
		self.axioms.append(["assert", [
			"not",
			[
				"and",
				["is-str", t],
				["is-snil", ["sval", t]]
			]
		]])

	def nonempty_bitstr(self, term1, term2, term):
		"""
		Asserts that: term is an nonempty bitstring.
		"""
		t = self.decode(term)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["is-str", t],
			["is-scons", ["sval", t]],
			["is-bool", t1],
			["=", ["bval", t1], ["shd", ["sval", t]]],
			["is-str", t2],
			["=", ["sval", t2], ["stl", ["sval", t]]],
		]])

	def nonempty_bitstr_reversed(self, term1, term2, term):
		"""
		Asserts that: Not (term is a nonempty bitstring).
		"""
		t = self.decode(term)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"not",
			[
				"and",
				["is-str", t],
				["is-scons", ["sval", t]]
			]
		]])

	# TODO bitmatch_*_*_reversed methods negate only the last assertion

	def bitmatch_const_true(self, termRest, cnstValue, size, termBitstr):
		"""
		Asserts that: termBitstr == <<cnstValue/size, termRest>>.
		"""
		r = self.decode(termRest)
		n = self.decode(cnstValue)
		b = self.decode(size)
		t = self.decode(termBitstr)
		slist = self.slist_const_pair(n, b)
		self.axioms.append(["assert", ["and", ["is-str", r], ["is-str", t]]])
		self.axioms.append(["assert", ["=", ["slist_concat", slist, ["sval", r]], ["sval", t]]])

	def bitmatch_const_true_reversed(self, termRest, cnstValue, size, termBitstr):
		"""
		Asserts that: Not (termBitstr == <<cnstValue/size, termRest>>).
		"""
		r = self.decode(termRest)
		n = self.decode(cnstValue)
		b = self.decode(size)
		t = self.decode(termBitstr)
		slist = self.slist_const_pair(n, b)
		self.axioms.append(["assert", ["and", ["is-str", r], ["is-str", t]]])
		self.axioms.append(["assert", ["not", ["=", ["slist_concat", slist, ["sval", r]], ["sval", t]]]])

	def bitmatch_const_false(self, cnstValue, size, termBitstr):
		"""
		Asserts that: termBitstr =/= <<cnstValue/size, termRest>>.
		"""
		n = self.decode(cnstValue)
		b = self.decode(size)
		t = self.decode(termBitstr)
		slist = self.slist_const_pair(n, b)
		self.axioms.append(["assert", ["is-str", t]])
		self.axioms.append(["assert", ["not", ["slist_match", slist, ["sval", t]]]])

	def bitmatch_const_false_reversed(self, cnstValue, size, termBitstr):
		"""
		Asserts that: Not (termBitstr =/= <<cnstValue/size, termRest>>).
		"""
		n = self.decode(cnstValue)
		b = self.decode(size)
		t = self.decode(termBitstr)
		slist = self.slist_const_pair(n, b)
		self.axioms.append(["assert", ["is-str", t]])
		self.axioms.append(["assert", ["slist_match", slist, ["sval", t]]])

	def bitmatch_var_true(self, term1, term2, size, termBitstr):
		"""
		Asserts that: termBitstr == <<term1/size, term2>>.
		"""
		r = self.decode(term2)
		n = self.decode(term1)
		b = self.decode(size)
		t = self.decode(termBitstr)
		slist = self.slist_var_pair(n, b)
		self.axioms.append(["assert", ["and", ["is-str", r], ["is-str", t]]])
		self.axioms.append(["assert", ["=", ["slist_concat", slist, ["sval", r]], ["sval", t]]])

	def bitmatch_var_true_reversed(self, term1, term2, size, termBitstr):
		"""
		Asserts that: Not (termBitstr == <<term1/size, term2>>).
		"""
		r = self.decode(term2)
		n = self.decode(term1)
		b = self.decode(size)
		t = self.decode(termBitstr)
		slist = self.slist_var_pair(n, b)
		self.axioms.append(["assert", ["and", ["is-str", r], ["is-str", t]]])
		self.axioms.append(["assert", ["not", ["=", ["slist_concat", slist, ["sval", r]], ["sval", t]]]])

	def bitmatch_var_false(self, size, termBitstr):
		"""
		Asserts that: termBitstr =/= <<term1/size, term2>>.
		"""
		b = self.decode(size)
		t = self.decode(termBitstr)
		assert isinstance(b, list) and len(b) == 2 and b[0] == "int", "b must be a concrete integer decoded term"
		b = calculate_int(b[1])
		assert b >= 0, "b must be a non-negative integer"
		axioms = ["and"]
		slist = ["sval", t]
		while b > 0:
			axioms.append(["not", ["is-snil", slist]])
			slist = ["stl", slist]
			b -= 1
		self.axioms.append(["assert", ["is-str", t]])
		self.axioms.append(["assert", ["not", axioms]])

	def bitmatch_var_false_reversed(self, size, termBitstr):
		"""
		Asserts that: Not (termBitstr =/= <<term1/size, term2>>).
		"""
		b = self.decode(size)
		t = self.decode(termBitstr)
		assert isinstance(b, list) and len(b) == 2 and b[0] == "int", "b must be a concrete integer decoded term"
		b = calculate_int(b[1])
		assert b >= 0, "b must be a non-negative integer"
		axioms = ["and"]
		slist = ["sval", t]
		while b > 0:
			axioms.append(["not", ["is-snil", slist]])
			slist = ["stl", slist]
			b -= 1
		self.axioms.append(["assert", ["is-str", t]])
		self.axioms.append(["assert", axioms])

	def lambda_with_arity(self, tFun, tArity):
		"""
		Asserts that: tFun is a function with arity tArity.
		"""
		f = self.decode(tFun)
		a = self.decode(tArity)
		self.axioms.append(["assert", [
			"and",
			["is-fun", f],
			["is-int", a],
			["=", ["arity", ["fval", f]], ["ival", a]],
		]])

	def not_lambda_with_arity_reversed(self, tFun, tArity):
		"""
		Asserts that: Not (tFun is not a function with arity tArity).
		"""
		self.lambda_with_arity(tFun, tArity)

	# -------------------------------------------------------------------------
	# Erlang BIFs or MFAs treated as BIFs.
	# -------------------------------------------------------------------------

	def head(self, term0, term1):
		"""
		Asserts that: term0 == hd(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["is-list", t1],
			["is-cons", ["lval", t1]],
			["=", t0, ["hd", ["lval", t1]]],
		]])

	def tail(self, term0, term1):
		"""
		Asserts that: term0 == tl(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["is-list", t1],
			["is-cons", ["lval", t1]],
			["=", t0, ["list", ["tl", ["lval", t1]]]],
		]])

	def cons(self, term0, term1, term2):
		"""
		Asserts that: term0 = [term1 | term2].
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["is-list", t2],
			["=", t0, ["list", ["cons", t1, ["lval", t2]]]],
		]])

	### Operations on atoms.
	# TODO replace booleans as true != atomTrue and false != atomFalse

	def atom_nil(self, term0, term1):
		"""
		Asserts that: term0 = (term1 == '').
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["=", t1, ["atom", "inil"]]]]])

	def atom_head(self, term0, term1):
		"""
		Asserts that: term0 is the first character of term1.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["is-atom", t1],
			["is-icons", ["aval", t1]],
			["=", t0, ["int", ["ihd", ["aval", t1]]]],
		]])

	def atom_tail(self, term0, term1):
		"""
		Asserts that: term0 is term1 without its first character.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["is-atom", t1],
			["is-icons", ["aval", t1]],
			["=", t0, ["atom", ["itl", ["aval", t1]]]],
		]])

	### Operations on tuples.

	def tcons(self, *terms):
		"""
		Asserts that: a term is tuple of many terms.
		"""
		t0 = self.decode(terms[0])
		tlist = "nil"
		for term in reversed(terms[1:]):
			t = self.decode(term)
			tlist = ["cons", t, tlist]
		self.axioms.append(["assert", ["=", t0, ["tuple", tlist]]])

	### Query types.

	def is_boolean(self, term0, term1):
		"""
		Asserts that: term0 == is_boolean(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["is-bool", t1]]]])

	def is_integer(self, term0, term1):
		"""
		Asserts that: term0 == is_integer(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["is-int", t1]]]])

	def is_float(self, term0, term1):
		"""
		Asserts that: term1 == is_float(term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["is-real", t1]]]])

	def is_list(self, term0, term1):
		"""
		Asserts that: term0 == is_list(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["is-list", t1]]]])

	def is_tuple(self, term0, term1):
		"""
		Asserts that: term0 == is_tuple(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["is-tuple", t1]]]])

	def is_atom(self, term0, term1):
		"""
		Asserts that: term0 == is_atom(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["or", ["is-bool", t1], ["is-atom", t1]]]]])

	def is_bitstring(self, term0, term1):
		"""
		Asserts that: term0 == is_bitstring(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["is-str", t1]]]])

	def is_fun(self, term0, term1):
		"""
		Asserts that: term0 == is_function(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["is-fun", t1]]]])

	def is_fun_with_arity(self, r, t, a):
		"""
		Asserts that: r == is_function(t, a).
		"""
		r = self.decode(r)
		t = self.decode(t)
		a = self.decode(a)
		self.axioms.append(["assert", ["=", r, ["bool", [
			"and",
			["is-fun", t],
			["is-int", a],
			["=", ["arity", ["fval", t]], ["ival", a]],
		]]]])

	def is_number(self, term0, term1):
		"""
		Asserts that: term0 == is_number(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, ["bool", ["or", ["is-int", t1], ["is-real", t1]]]]])

	### Arithmetic Operations.

	def _binary_operation(self, operator, term0, term1, term2):
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["or", ["is-int", t1], ["is-real", t1]],
			["or", ["is-int", t2], ["is-real", t2]],
			[
				"ite",
				["and", ["is-int", t1], ["is-int", t2]],
				["=", t0, ["int", [operator, ["ival", t1], ["ival", t2]]]],
				[
					"=",
					t0,
					[
						"real",
						[
							operator,
							["ite", ["is-int", t1], ["to_real", ["ival", t1]], ["rval", t1]],
							["ite", ["is-int", t2], ["to_real", ["ival", t2]], ["rval", t2]]
						]
					]
				]
			],
		]])

	def plus(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 + term2.
		"""
		self._binary_operation("+", term0, term1, term2)

	def minus(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 - term2.
		"""
		self._binary_operation("-", term0, term1, term2)

	def times(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 * term2.
		"""
		self._binary_operation("*", term0, term1, term2)

	def rdiv(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 / term2.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["or", ["is-int", t1], ["is-real", t1]],
			[
				"or",
				["and", ["is-int", t2], ["not", ["=", ["ival", t2], "0"]]],
				["and", ["is-real", t2], ["not", ["=", ["rval", t2], "0"]]],
			],
			[
				"ite",
				["and", ["is-int", t1], ["is-int", t2]],
				["=", t0, ["int", ["div", ["ival", t1], ["ival", t2]]]], # TODO cuter_z3.py returns a real when dividing two integers?
				["=", t0, ["real", [
					"/",
					["ite", ["is-int", t1], ["to_real", ["ival", t1]], ["rval", t1]],
					["ite", ["is-int", t2], ["to_real", ["ival", t2]], ["rval", t2]]
				]]]
			],
		]])
		# TODO solver returns unknown when there are no other constraints; nonlinear integer arithmetic is undecidable

	def idiv_nat(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 // term2.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["is-int", t1],
			["is-int", t2],
			[">=", ["ival", t1], "0"],
			[">", ["ival", t2], "0"],
			["=", t0, ["int", ["div", ["ival", t1], ["ival", t2]]]],
		]])

	def rem_nat(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 % term2.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["is-int", t1],
			["is-int", t2],
			[">=", ["ival", t1], "0"],
			[">", ["ival", t2], "0"],
			["=", t0, ["int", ["mod", ["ival", t1], ["ival", t2]]]],
		]])

	def unary(self, term0, term1):
		"""
		Asserts that: term0 = - term1.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["or", ["is-int", t1], ["is-real", t1]],
			[
				"ite",
				["is-int", t1],
				["=", t0, ["int", ["-", ["ival", t1]]]],
				["=", t0, ["real", ["-", ["rval", t1]]]]
			],
		]])

	# TODO currently term0 must be a real and term2 an integer
	def pow(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 ** term2.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["is-real", t0],
			["or", ["is-int", t1], ["is-real", t1]],
			["is-int", t2],
			["real-pow", ["rval", t0], ["ite", ["is-int", t1], ["to_real", ["ival", t1]], ["rval", t1]], ["ival", t2]],
		]])

	def trunc(self, term0, term1):
		"""
		Asserts that: term0 is term1 truncated.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["or", ["is-int", t1], ["is-real", t1]],
			[
				"=",
				t0,
				[
					"ite",
					["is-int", t1],
					t1,
					[
						"int",
						[
							"ite",
							[">=", ["rval", t1], "0.0"],
							["to_int", ["rval", t1]],
							["-", ["to_int", ["-", ["rval", t1]]]]
						]
					]
				]
			],
		]])

	### Comparisons.

	def equal(self, term0, term1, term2):
		"""
		Asserts that: term0 = (term1 == term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["=", t0, ["bool", ["=", t1, t2]]]])

	def lt_integers(self, term0, term1, term2):
		"""
		Asserts that: term0 = (term1 < term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["is-int", t1],
			["is-int", t2],
			["=", t0, ["bool", ["<", ["ival", t1], ["ival", t2]]]],
		]])

	def lt_floats(self, term0, term1, term2):
		"""
		Asserts that: term0 = (term1 < term2).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", [
			"and",
			["is-real", t1],
			["is-real", t2],
			["=", t0, ["bool", ["<", ["rval", t1], ["rval", t2]]]],
		]])

	### Type conversions.

	def to_float(self, term0, term1):
		"""
		Asserts that: term0 = float(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["or", ["is-int", t1], ["is-real", t1]],
			["=", t0, ["real", ["ite", ["is-int", t1], ["to_real", ["ival", t1]], ["rval", t1]]]],
		]])

	def list_to_tuple(self, term0, term1):
		"""
		Asserts that: term0 = list_to_tuple(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["is-list", t1],
			["=", t0, ["tuple", ["lval", t1]]],
		]])

	def tuple_to_list(self, term0, term1):
		"""
		Asserts that: term0 = tuple_to_list(term1).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", [
			"and",
			["is-tuple", t1],
			["=", t0, ["list", ["tval", t1]]],
		]])

	### Bogus operations (used for their side-effects in Erlang).

	def bogus(self, term0, term1):
		"""
		Asserts that: term0 == term1 (Identity function).
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		self.axioms.append(["assert", ["=", t0, t1]])

	### Bitwise Operations.

	def band(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 & term2.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["int-and", ["ival", t1], ["ival", t2], ["ival", t0]]])

	def bxor(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 ^ term2.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["int-xor", ["ival", t1], ["ival", t2], ["ival", t0]]])

	def bor(self, term0, term1, term2):
		"""
		Asserts that: term0 = term1 | term2.
		"""
		t0 = self.decode(term0)
		t1 = self.decode(term1)
		t2 = self.decode(term2)
		self.axioms.append(["assert", ["int-or", ["ival", t1], ["ival", t2], ["ival", t0]]])
