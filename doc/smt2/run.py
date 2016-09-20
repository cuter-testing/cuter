#!/usr/bin/python3

from parser import *
import fileinput

smt = ""
for line in fileinput.input():
	smt += line

root = SMT_List.construct(smt)

root.simplify()
