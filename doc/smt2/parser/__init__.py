from collections import deque

class SMT_Node:
	def __init__(self, smt, cur):
		while smt[cur].isspace():
			cur += 1
		self.beg = cur
		while smt[cur] != ")" and not smt[cur].isspace():
			cur += 1
		self.end = cur
		self.smt = smt[self.beg:self.end]
	def print(self, depth = 0):
		print("\t" * depth + self.smt)
	def simplify(self, depth = 0):
		self.print(depth)
	def value(self, table = {}):
		if self.smt == "inil":
			return deque()
		elif self.smt in table:
			return table[self.smt]

class SMT_List(SMT_Node):
	"a SMT_List begins with an opening parenthesis and ends with a closing parenthesis"
	
	def _preview(smt, cur):
		"find the type of a list without parsing it"
		assert smt[cur] == "(", "list must begin with an opening parenthesis"
		cur += 1
		while smt[cur].isspace():
			cur += 1
		if smt[cur] == "(":
			return
		if smt[cur] == ")":
			return
		beg = cur
		while smt[cur] != ")" and not smt[cur].isspace():
			cur += 1
		return smt[beg:cur]

	def construct(smt, cur = 0):
		assert smt[cur] == "(", "list must begin with an opening parenthesis"
		t = SMT_List._preview(smt, cur)
		if t == "model":
			return SMT_Model(smt, cur)
		elif t == "define-fun":
			return SMT_Define_Fun(smt, cur)
		elif t == "let":
			return SMT_Let(smt, cur)
		elif t == "atom":
			return SMT_Atom(smt, cur)
		elif t == "int":
			return SMT_Int(smt, cur)
		elif t == "real":
			return SMT_Real(smt, cur)
		elif t == "icons":
			return SMT_ICons(smt, cur)
		else:
			return SMT_List(smt, cur)

	def __init__(self, smt, cur):
		assert smt[cur] == "(", "list must begin with an opening parenthesis"
		self.beg = cur
		self.nodes = []
		cur += 1
		while True:
			while smt[cur].isspace():
				cur += 1
			if smt[cur] == ")":
				break
			if smt[cur] == "(":
				anode = SMT_List.construct(smt, cur)
			else:
				anode = SMT_Node(smt, cur)
			self.nodes.append(anode)
			cur = anode.end
		self.end = cur + 1
		self.smt = smt[self.beg:self.end]

	def print(self, depth = 0):
		print("\t" * depth + "(")
		for smt_node in self.nodes:
			smt_node.print(depth + 1)
		print("\t" * depth + ")")

	def value(self):
		pass

	def simplify(self, depth = 0):
		value = self.value()
		if value is None:
			print("\t" * depth + "(")
			for smt_node in self.nodes:
				smt_node.simplify(depth + 1)
			print("\t" * depth + ")")
		else:
			print("\t" * depth + str(value))

class SMT_Model(SMT_List):
	def value(self):
		pass

class SMT_Define_Fun(SMT_List):
	def value(self):
		pass

class SMT_Let(SMT_List):
	def value(self, table = {}):
		for var in self.nodes[1].nodes:
			table[var.nodes[0].smt] = var.nodes[1].value(table)
		return self.nodes[2].value(table)

class SMT_Atom(SMT_List):
	def value(self, table = {}):
		return self.nodes[1].value(table)

class SMT_Int(SMT_List):
	def value(self):
		return int(self.nodes[1].smt)

class SMT_Real(SMT_List):
	def value(self):
		# TODO what's going on with real?
		nodes = self.nodes[1].nodes
		return float(nodes[1].smt) / float(nodes[2].smt)

class SMT_ICons(SMT_List):
	def value(self, table):
		value = int(self.nodes[1].smt)
		queue = self.nodes[2].value(table)
		queue.appendleft(value)
		return queue;
