from parser.group import Group

class Func(Group):
	def simplify(self, level = 0):
		if self.nodes[2].nodes:
			self.print(level)
		else:
			name = self.nodes[1].smt
			t = self.nodes[3].smt
			value = self.nodes[4].value({})
			print("\t" * level + "{} ({}): {}".format(name, t, str(value)))
