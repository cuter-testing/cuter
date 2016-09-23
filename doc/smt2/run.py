#!/usr/bin/python3

import parser
import fileinput

smt = ""
for line in fileinput.input():
	smt += line

root = parser.parse(smt)

root.simplify()
