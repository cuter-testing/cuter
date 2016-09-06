#!/usr/bin/env python
# -*- coding: utf-8 -*-

import fileinput
import parser

smt = ""
for line in fileinput.input() :
	smt += line
nodes = parser.parse( smt )
parser.print_nodes( nodes )
