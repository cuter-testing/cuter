from parser.list import List

class Real(List):
	def value(self, table = {}):
		# TODO what's going on with real?
		nodes = self.nodes[1].nodes
		return float(nodes[1].smt) / float(nodes[2].smt)
