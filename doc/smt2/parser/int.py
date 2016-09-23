from parser.list import List

class Int(List):
	def value(self, table = {}):
		return int(self.nodes[1].smt)
