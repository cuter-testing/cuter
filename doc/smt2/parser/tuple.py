from parser.group import Group

class Tuple(Group):
	def value(self, table = {}):
		return tuple(self.nodes[1].value(table))
