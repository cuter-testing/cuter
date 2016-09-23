from parser.group import Group

class Atom(Group):
	def value(self, table = {}):
		return self.nodes[1].value(table)
