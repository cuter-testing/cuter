import fileinput

def get_ncp( l ) :
	ncp = smt.find( ")", l )
	if ncp < 0 :
		sys.exit( "closing parenthesis not found" )
	return ncp

def get_nop( l, u ) :
	return smt.find( "(", l, u )

def parse_node( l, ncp ) :
	nodes = [];
	nop = get_nop( l, ncp )
	while nop >= 0 :
		nodes.extend( smt[l:nop].split() )
		res = parse_node( nop+1, ncp )
		nodes.append( res[0] )
		l = res[1]
		ncp = get_ncp( l )
		nop = get_nop( l, ncp )
	nodes.extend( smt[l:ncp].split() )
	return ( nodes, ncp+1 )

smt = ""
for line in fileinput.input() :
	smt += line

nop = smt.find( "(" );
if nop < 0 :
	sys.exit( "opening parenthesis not found" )
nodes = parse_node( nop+1, get_ncp( nop+1 ) )[0]

def print_nodes( nodes, level ) :
	for node in nodes :
		if isinstance( node, str ) :
			print "\t" * level + node
		else :
			print "\t" * level + "("
			print_nodes( node, level + 1 )
			print "\t" * level + ")"

print_nodes( nodes, 0 )
