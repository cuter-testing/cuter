from parser.node import Node
from parser.leaf import Leaf
from parser.group import Group
from parser.model import Model
from parser.func import Func
from parser.let import Let
from parser.icons import Icons
from parser.atom import Atom
from parser.real import Real
from parser.int import Int
from parser.cons import Cons
from parser.tuple import Tuple
from parser.list import List
from parser.ite import Ite

def preview_type(smt, cur):
	"find the type of a node without parsing it"
	if smt[cur] != "(":
		return "_leaf"
	# smt begins with an opening parenthesis
	cur += 1
	# ignore white spaces
	while smt[cur].isspace():
		cur += 1
	# check whether group is empty
	if smt[cur] == ")":
		return None
	# check whether first item is a group
	if smt[cur] == "(":
		return None
	# type is a word
	# save beginning position of word
	beg = cur
	# loop until end of word
	while smt[cur] != ")" and not smt[cur].isspace():
		cur += 1
	# return the word
	return smt[beg:cur]

def parse(smt, cur = 0):
	t = preview_type(smt, cur)
	if t == "_leaf":
		return Leaf(smt, cur)
	elif t == "model":
		return Model(smt, cur)
	elif t == "define-fun":
		return Func(smt, cur)
	elif t == "let":
		return Let(smt, cur)
	elif t == "icons":
		return Icons(smt, cur)
	elif t == "atom":
		return Atom(smt, cur)
	elif t == "real":
		return Real(smt, cur)
	elif t == "int":
		return Int(smt, cur)
	elif t == "cons":
		return Cons(smt, cur)
	elif t == "tuple":
		return Tuple(smt, cur)
	elif t == "list":
		return List(smt, cur)
	elif t == "ite":
		return Ite(smt, cur)
	else:
		return Group(smt, cur)

def compare(node1, node2):
	if node1.is_leaf:
		if node2.is_leaf:
			return node1.smt == node2.smt
		else:
			return False
	else:
		if node2.is_leaf:
			return False
		else:
			len1 = len(node1.nodes)
			len2 = len(node2.nodes)
			if len1 != len2:
				return False
			for cnt in range(len1):
				if not compare(node1.nodes[cnt], node2.nodes[cnt]):
					return False
			return True
