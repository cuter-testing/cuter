# -*- coding: utf-8 -*-

import unittest
import parser
from test_utils import *

class ParserTestcase( unittest.TestCase ):

	def test_parse( self ):
		smt = load_asset("sample1.out").replace( "\n", "" )
		nodes = parser.parse( smt )
		self.assertEqual( 5, len( nodes ) )
