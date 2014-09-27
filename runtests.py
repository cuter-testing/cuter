#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys, os

if len(sys.argv) < 2:
  print "Too few arguments..."
  sys.exit(0)

ebin = sys.argv[1]
tests = ["cuter", "cuter_codeserver", "cuter_cerl", "cuter_monitor", "cuter_iserver", "cuter_eval", "cuter_json"]
for t in tests:
  print "Testing %s..." % t
  os.system("erl -noshell -pa %s -pa ../meck/ebin -eval \"eunit:test(%s, [verbose])\" -s init stop" % (ebin, t))
