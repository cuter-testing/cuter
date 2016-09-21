from collections import deque

class SMT_Node:
	def __init__(self, smt, cur):
		while smt[cur].isspace():
			cur += 1
		self.beg = cur
		while smt[cur] != ")" and not smt[cur].isspace():
			cur += 1
		self.end = cur
		self.smt = smt[self.beg:self.end]
	def print(self, depth = 0):
		print("\t" * depth + self.smt)
	def simplify(self, depth = 0):
		self.print(depth)
	def value(self, table = {}):
		if self.smt == "inil":
			return deque()
		elif self.smt in table:
			return table[self.smt]
