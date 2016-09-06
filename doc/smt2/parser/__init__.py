# -*- coding: utf-8 -*-

import sys
import core, helper

def parse( smt ):
	nop = smt.find( "(" );
	if nop < 0 :
		sys.exit( "opening parenthesis not found" )
	return core.parse_node( smt, nop+1, core.get_ncp( smt, nop+1 ) )[0]

def print_nodes( nodes ):
	helper.print_nodes( nodes, 0 )
