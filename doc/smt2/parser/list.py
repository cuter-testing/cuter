from parser.group import Group

class List(Group):
	def value(self, table = {}):
		return list(self.nodes[1].value(table))
