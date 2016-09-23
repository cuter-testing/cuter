from parser.group import Group

class Let(Group):
	def value(self):
		table = {}
		for node in self.nodes[1].nodes:
			name = node.nodes[0].smt
			value = node.nodes[1].value(table)
			table[name] = value
		return self.nodes[2].value(table)
