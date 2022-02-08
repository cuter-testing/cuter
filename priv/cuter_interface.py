# -*- coding: utf-8 -*-

import traceback, argparse
import cuter_global as cglb
import cuter_port as cp
import cuter_io as cIO
import cuter_smt as cSMT
import cuter_logger as clg
import cuter_common as cc

## Parse the arguments.

parser = argparse.ArgumentParser(description="Run the Solver Component.")
parser.add_argument("-d", "--debug", action='store_true', help="record debug information")
parser.add_argument("-s", "--smt", action='store_true', help="use the SMTv2 backend")
parser.add_argument("-t", "--timeout", metavar="N", type=int, default=2,
    help="set the timeout N for the SMT solver (default: %(default)s)")
args = parser.parse_args()

## Main Program

cglb.init(debug=args.debug)
# Initialize the communication with Erlang
erlport = cp.ErlangPort()
# Initialize the Solver interface
erlSolver = cSMT.ErlangSMT(args.timeout)

try:
    while cglb.__RUN__:
        data = erlport.receive()
        clg.data_received(data)
        cmd = cp.decode_command(erlport, erlSolver, data)
    clg.clean_empty_logs()

except:
    e = traceback.format_exc()
    erlport.send(e)
