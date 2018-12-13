#!/usr/bin/env python
# -*- coding: utf-8 -*-

import traceback, argparse
import cuter_global as cglb
import cuter_port as cp
import cuter_io as cIO
import cuter_logger as clg
import cuter_common as cc
import cuter_coordinator

## Parse the arguments.

parser = argparse.ArgumentParser(description="Run the Solver Component.")
parser.add_argument("-t", "--timeout", metavar="N", type=int, default=2,
    help="set the timeout N for the SMT solver (default: %(default)s)")
args = parser.parse_args()

## Main Program

cglb.init()
# Initialize the communication with Erlang
erlport = cp.ErlangPort()
# Initialize the Solver interface
coordinator = cuter_coordinator.Solver_Coordinator_Z3(args.timeout)

try:
    while cglb.__RUN__:
        data = erlport.receive()
        clg.data_received(data)
        cmd = cp.decode_command(erlport, coordinator, data)
    clg.clean_empty_logs()

except:
    e = traceback.format_exc()
    erlport.send(e)
