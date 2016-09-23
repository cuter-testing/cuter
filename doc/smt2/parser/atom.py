from parser.list import List

class Atom(List):
	def value(self, table = {}):
		return self.nodes[1].value(table)
