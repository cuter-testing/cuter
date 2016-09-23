from parser.group import Group

class Model(Group):
	def simplify(self, level = 0):
		print("\t" * level + "(")
		for node in self.nodes:
			node.simplify(level + 1)
		print("\t" * level + ")")
