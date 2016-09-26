import parser
from parser.node import Node

class Group(Node):
	def __init__(self, smt, cur):
		self.is_leaf = False
		assert smt[cur] == "(", "group must begin with an opening parenthesis"
		self.beg = cur
		self.nodes = []
		cur += 1
		while True:
			while smt[cur].isspace():
				cur += 1
			if smt[cur] == ")":
				break
			node = parser.parse(smt, cur)
			self.nodes.append(node)
			cur = node.end
		assert smt[cur] == ")", "group must end with a closing parenthesis"
		self.end = cur + 1
		self.smt = smt[self.beg:self.end]
	def print(self, level = 0):
		print("\t" * level + "(")
		for node in self.nodes:
			node.print(level + 1)
		print("\t" * level + ")")