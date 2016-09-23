from parser.group import Group

class Int(Group):
	def value(self, table = {}):
		return int(self.nodes[1].smt)
