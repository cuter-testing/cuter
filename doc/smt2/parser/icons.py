from parser.list import List

class Icons(List):
	def value(self, table = {}):
		queue = self.nodes[2].value(table)
		queue.appendleft(self.nodes[1].smt)
		return queue
