from parser.group import Group

class Func(Group):
	def simplify(self, level = 0):
		if self.nodes[2].nodes:
			self.print(level)
		else:
			print("\t" * level + "{} ({}): {}".format(self.nodes[1].smt, self.nodes[3].smt, str(self.nodes[4].value())))
