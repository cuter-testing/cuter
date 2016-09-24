#!/usr/bin/python3

import argparse
argparser = argparse.ArgumentParser()
argparser.add_argument(
	"-f", "--file", type=str,
	help="input file (omit to read from standard input)"
)
args = argparser.parse_args()
if args.file is None:
	import sys
	smt = sys.stdin.read()
	sys.stdin.close()
else:
	f = open(args.file, "r")
	smt = f.read()
	f.close()

import parser
root = parser.parse(smt)
root.simplify()
