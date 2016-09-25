from parser.group import Group

class Real(Group):
	def value(self, table = {}):
		if self.nodes[1].is_leaf:
			return float(self.nodes[1].smt)
		else:
			nodes = self.nodes[1].nodes
			assert len(nodes) == 3, "invalid number of operands"
			assert nodes[0].smt == "/", "unknown operation"
			return float(nodes[1].smt) / float(nodes[2].smt)
