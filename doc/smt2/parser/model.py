from parser.list import List

class Model(List):
	def simplify(self, level = 0):
		print("\t" * level + "(")
		for node in self.nodes:
			node.simplify(level + 1)
		print("\t" * level + ")")
