class Node:
	"basic node of SMT tree"
	# smt, beg, end
	# __init__(), value()
	def __init__(self, smt, cur):
		assert False, "extend class Group or Leaf"
	def value(self, table = {}):
		assert False, self.smt
	def print(self, level = 0):
		print("\t" * level + self.smt)
	def simplify(self, level = 0):
		self.print(level)
