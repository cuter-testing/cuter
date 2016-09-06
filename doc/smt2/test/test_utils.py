# -*- coding: utf-8 -*-

assets_directory = "test/assets/"

def load_asset( fname ):
	with open( assets_directory + fname, "r" ) as f:
		return f.read()
