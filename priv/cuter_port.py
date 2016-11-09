#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, struct, json
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc
import cuter_io as cIO
import cuter_proto_solver_command_pb2 as scmd

class ErlangPort:
    def __init__(self):
        self.chan_in = sys.stdin
        self.chan_out = sys.stdout

    def receive(self):
        """
        Receives a command from Erlang.
        """
        x = self.chan_in.read(4)
        if (len(x) == 4):
            sz = struct.unpack('!i', x)[0]
            return self.chan_in.read(sz)

    def send(self, data):
        """
        Sends data to Erlang.
        """
        if cglb.__TTY__:
            print data
        else:
          try:
              sz = len(data)
              x = struct.pack('!i', sz)
              self.chan_out.write(x)
              return self.chan_out.write(data)
          except:
              err = open('error.log', 'a')
              err.write(str(data) + ",\n")
              err.flush()
              err.close()

def decode_command(erlport, erlSolver, data):
    cmd = scmd.SolverCommand()
    cmd.ParseFromString(data)
    opts = {
        scmd.SolverCommand.LOAD_TRACE_FILE: decode_load_trace_file,
        scmd.SolverCommand.SOLVE: decode_solve,
        scmd.SolverCommand.GET_MODEL: decode_get_model,
        scmd.SolverCommand.ADD_AXIOMS: decode_add_axioms,
        scmd.SolverCommand.FIX_VARIABLE: decode_fix_variable,
        scmd.SolverCommand.RESET_SOLVER: decode_reset_solver,
        scmd.SolverCommand.STOP: decode_stop,
    }
    opts[cmd.type](erlport, erlSolver, cmd)

def decode_load_trace_file(erlport, erlSolver, cmd):
    """
    Loads a trace file.
    """
    r = cIO.JsonReader(cmd.filename, cmd.to_constraint)
    for entry, rev in r:
        if cc.is_interpretable(entry):
            erlSolver.command_toSolver(entry, rev)

def decode_solve(erlport, erlSolver, cmd):
    """
    Solves the model.
    """
    slv = erlSolver.solve()
    erlport.send(slv.SerializeToString())

def decode_get_model(erlport, erlSolver, cmd):
    """
    Gets the model.
    """
    enc = erlSolver.encode_model()
#    md = erlSolver.model
#    erlport.send(str(md))
    erlport.send(enc.SerializeToString())

def decode_add_axioms(erlport, erlSolver, cmd):
    """
    Adds the axioms.
    """
    erlSolver.add_axioms()

def decode_fix_variable(erlport, erlSolver, cmd):
    """
    Fixes a variable to a specific value.
    """
    var, val = cmd.symbvar, cmd.symbvar_value
    erlSolver.fix_parameter(var, val)

def decode_reset_solver(erlport, erlSolver, cmd):
    """
    Resets the solver.
    """
    erlSolver.reset_solver()

def decode_stop(erlport, erlSolver, cmd):
    """
    Stops the execution.
    """
    cglb.__RUN__ = False
