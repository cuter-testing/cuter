# -*- coding: utf-8 -*-

def print_nodes( nodes, level ) :
	for node in nodes :
		if isinstance( node, str ) :
			print "\t" * level + node
		else :
			print "\t" * level + "("
			print_nodes( node, level + 1 )
			print "\t" * level + ")"
