from parser.leaf import Leaf
from parser.group import Group

class Int(Group):
	def value(self, table = {}):
		if isinstance(self.nodes[1], Leaf):
			return int(self.nodes[1].smt)
		else:
			nodes = self.nodes[1].nodes
			assert len(nodes) == 2, "invalid number of operands"
			assert nodes[0].smt == "-", "unknown operation"
			return -int(nodes[1].smt)
