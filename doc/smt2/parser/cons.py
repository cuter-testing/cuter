from parser.group import Group

class Cons(Group):
	def value(self, table = {}):
		queue = self.nodes[2].value(table)
		item = self.nodes[1].value(table)
		queue.appendleft(item)
		return queue
