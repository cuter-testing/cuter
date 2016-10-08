#!/usr/bin/env python
# -*- coding: utf-8 -*-

##
## Solves the constraints in a trace file offline.
##

import sys, getopt, os
import subprocess as subp
import cuter_io as cIO
import cuter_z3 as cz3
import cuter_smt as cSMT
import cuter_common as cc
import cuter_print as cpt
import cuter_global as cglb

def solve(fname, n, printConstraints, withSmt, withPrint):
    # Do the initializations.
    cglb.init()
    erlSolver = cz3.ErlangZ3() if not withSmt else cSMT.ErlangSMT()
    # Load the trace.
    r = cIO.JsonReader(fname, n)
    for tp, tag, x, rev in r:
        if printConstraints:
            cpt.print_cmd(tp, tag, x, rev)
        if cc.is_interpretable(tp):
            erlSolver.command_toSolver(tp, x, rev)
    # Load the axioms to the solver.
    erlSolver.add_axioms()
    # Solve the model.
    if withPrint: print "Solving the model ...",
    slv = erlSolver.solve()
    if withPrint: print slv
    if slv == "sat":
        m = erlSolver.model
        if withPrint: print m
        return (slv, str(m))

def usage():
    print "Usage: cuter_solve_offline.py TraceFile NoOfConstraint [ options ]"
    print "PARAMETERS"
    print "	TraceFile		The trace file of an execution."
    print "	NoOfConstraint		The cardinality of the constraint to reverse."
    print "OPTIONS"
    print "	--print-constraints	Print all the commands as they are being loaded (for debugging)."
    print "	--smt			Uses the SMT backend."
    print "	-e Dir			Dir is the ebin."
    print "	-u Dir			Dir is the utest ebin."
    print "	--help			Display this information."

def run_tests(ebin, utestEbin):
    fname = "logfile"
    cmd = " ".join(["erl", "-noshell", "-pa", ebin, "-pa", utestEbin, "-eval",
        "\"cuter_tests_lib:sample_trace_file(\\\"{}\\\")\"".format(fname), "-s", "init", "stop"])
    subp.call(cmd, shell=True)
    assert ("sat", "[x2 = int(44), x1 = int(1)]") == solve(fname, 3, False, False, False), "Solver error"
    try:
        os.remove(fname)
    except OSError:
        pass

if __name__ == "__main__":
    shortOpts = "u:e:"
    longOpts = ["print-constraints", "help", "smt"]
    try:
        optlist, remainder = getopt.gnu_getopt(sys.argv[1:], shortOpts, longOpts)
        printConstraints, withSmt = False, False
        ebin, utestEbin = None, None
        for opt, arg in optlist:
            if opt == "--help":
                usage()
                sys.exit(0)
            elif opt == "--print-constraints":
                printConstraints = True
            elif opt == "--smt":
                withSmt = True
            elif opt == "-u":
                utestEbin = arg
            elif opt == "-e":
                ebin = arg
        if ebin != None or utestEbin != None:
            sys.exit(run_tests(ebin, utestEbin))
        solve(remainder[0], int(remainder[1]), printConstraints, withSmt, True)
    except Exception as e:
        print "Fatal Error:", e
        sys.exit(1)
