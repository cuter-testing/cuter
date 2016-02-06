#!/usr/bin/env python
# -*- coding: utf-8 -*-

##
## Solves the constraints in a trace file offline.
##

import sys, getopt
import cuter_io as cIO
import cuter_z3 as cz3
import cuter_common as cc
import cuter_print as cpt
import cuter_global as cglb

def solve(fname, n, printConstraints):
  # Do the initializations.
  cglb.init()
  erlz3 = cz3.ErlangZ3()
  # Load the trace.
  r = cIO.JsonReader(fname, n)
  for tp, tag, x, rev in r:
    if printConstraints:
      cpt.print_cmd(tp, tag, x, rev)
    if cc.is_interpretable(tp):
      erlz3.command_toZ3(tp, x, rev)
  # Load the axioms to the solver.
  erlz3.add_axioms()
  # Solve the model.
  print "Solving the model ...",
  slv = erlz3.solve()
  print slv
  if slv == "sat":
    print erlz3.model

def usage():
  print "Usage: cuter_solve_offline.py TraceFile NoOfConstraint [ options ]"
  print "PARAMETERS"
  print "	TraceFile		The trace file of an execution."
  print "	NoOfConstraint		The cardinality of the constraint to reverse."
  print "OPTIONS"
  print "	--print-constraints	Print all the commands as they are being loaded (for debugging)."
  print "	--help			Display this information."

if __name__ == "__main__":
  shortOpts = ""
  longOpts = ["print-constraints", "help"]
  try:
    optlist, remainder = getopt.gnu_getopt(sys.argv[1:], shortOpts, longOpts)
    printConstraints = False
    for opt, arg in optlist:
      if opt == "--help":
        usage()
        sys.exit(0)
      if opt == "--print-constraints":
        printConstraints = True
    solve(remainder[0], int(remainder[1]), printConstraints)
  except Exception as e:
    print "Fatal Error:", e
    sys.exit(1)
