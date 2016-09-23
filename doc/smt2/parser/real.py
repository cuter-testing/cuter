from parser.leaf import Leaf
from parser.group import Group

class Real(Group):
	def value(self, table = {}):
		# TODO what's going on with real?
		if isinstance(self.nodes[1], Leaf):
			return float(self.nodes[1].smt)
		else:
			nodes = self.nodes[1].nodes
			return float(nodes[1].smt) / float(nodes[2].smt)
