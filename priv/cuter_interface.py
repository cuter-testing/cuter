#!/usr/bin/env python
# -*- coding: utf-8 -*-

import traceback, argparse
import cuter_global as cglb
import cuter_port as cp
import cuter_io as cIO
import cuter_z3 as cz3
import cuter_smt as cSMT
import cuter_logger as clg
import cuter_common as cc

## Parse the arguments.

parser = argparse.ArgumentParser(description="Run the Solver Component.")
parser.add_argument("-s", "--smt", action='store_true', help="use the SMTv2 backend")
args = parser.parse_args()

## Main Program

cglb.init()
# Initialize the communication with Erlang
erlport = cp.ErlangPort()
# Initialize the Solver interface
erlSolver = cz3.ErlangZ3() if not args.smt else cSMT.ErlangSMT()

try:
    while cglb.__RUN__:
        data = erlport.receive()
        clg.data_received(data)
        cmd = cp.decode_command(erlport, erlSolver, data)
    clg.clean_empty_logs()

except:
    e = traceback.format_exc()
    erlport.send(e)
