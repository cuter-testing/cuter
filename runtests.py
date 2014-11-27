#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys, os

def run(mod, ebins):
  pas = " ".join(["-pa %s" % e for e in ebins])
  print "Testing %s..." % mod
  os.system("erl -noshell %s -eval \"eunit:test(%s, [verbose])\" -s init stop" % (pas, mod))

if len(sys.argv) < 2:
  print "Too few arguments..."
  sys.exit(0)

ebin = sys.argv[1]
meck = sys.argv[2] if len(sys.argv) >= 3 else None
tests = ["cuter", "cuter_cerl", "cuter_iserver", "cuter_eval", "cuter_json", "cuter_solver"]
tests_with_meck = ["cuter_codeserver", "cuter_monitor"]
for t in tests:
  run(t, [ebin])
if meck:
  for t in tests_with_meck:
    run(t, [ebin, meck])
