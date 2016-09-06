# -*- coding: utf-8 -*-

import sys

def get_ncp( smt, l ) :
	ncp = smt.find( ")", l )
	if ncp < 0 :
		sys.exit( "closing parenthesis not found" )
	return ncp

def get_nop( smt, l, u ) :
	return smt.find( "(", l, u )

def parse_node( smt, l, ncp ) :
	nodes = [];
	nop = get_nop( smt, l, ncp )
	while nop >= 0 :
		nodes.extend( smt[l:nop].split() )
		res = parse_node( smt, nop+1, ncp )
		nodes.append( res[0] )
		l = res[1]
		ncp = get_ncp( smt, l )
		nop = get_nop( smt, l, ncp )
	nodes.extend( smt[l:ncp].split() )
	return ( nodes, ncp+1 )

