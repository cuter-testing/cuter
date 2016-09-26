# -*- coding: utf-8 -*-

assets_directory = "test/assets/"

def load_asset( fname ):
	f = open( assets_directory + fname, "r")
	smt = f.read()
	f.close()
	return smt
