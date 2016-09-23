from parser.node import Node
from parser.leaf import Leaf
from parser.list import List
from parser.model import Model
from parser.func import Func
from parser.let import Let
from parser.icons import Icons
from parser.atom import Atom
from parser.real import Real
from parser.int import Int

def preview_type(smt, cur):
	"find the type of a list without parsing it"
	if smt[cur] != "(":
		return "_leaf"
	# smt begins with an opening parenthesis
	cur += 1
	# ignore white spaces
	while smt[cur].isspace():
		cur += 1
	# check whether list is empty
	if smt[cur] == ")":
		return None
	# check whether first item is a list
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
	else:
		return List(smt, cur)
