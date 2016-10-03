from parser.group import Group

class Fun(Group):
	def value(self, table = {}):
		return lambda: int(self.nodes[1].smt)
