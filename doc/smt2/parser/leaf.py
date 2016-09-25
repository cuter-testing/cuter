from parser.node import Node

from collections import deque

class Leaf(Node):
	def __init__(self, smt, cur):
		self.is_leaf = True
		while smt[cur].isspace():
			cur += 1
		self.beg = cur
		assert smt[cur] != ")", "leaf must not be empty"
		while smt[cur] != ")" and not smt[cur].isspace():
			cur += 1
		self.end = cur
		self.smt = smt[self.beg:self.end]
	def value(self, table = {}):
		if self.smt in table:
			return table[self.smt]
		elif self.smt == "nil" or self.smt == "inil" or self.smt == "bnil":
			return deque()
