from parser.group import Group

class Ite(Group):
	def value(self, table = {}):
		assert len(self.nodes[1].nodes), "invalid operation"
		assert self.nodes[1].nodes[0] == "=", "invalid operator"
		crit = self.nodes[1].nodes[1].value(table) == self.nodes[1].nodes[2].value(table)
		if crit:
			return self.nodes[2].value(table)
		else:
			return self.nodes[3].value(table)
