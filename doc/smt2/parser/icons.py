from parser.group import Group

class Icons(Group):
	def value(self, table = {}):
		queue = self.nodes[2].value(table)
		queue.appendleft(self.nodes[1].smt)
		return queue
