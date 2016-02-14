#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, struct, json
import cuter_global as cglb
import cuter_common as cc
import cuter_io as cIO

class ErlangPort:
  def __init__(self):
    self.chan_in = sys.stdin
    self.chan_out = sys.stdout
  
  # Receive a command from Erlang
  def receive(self):
    x = self.chan_in.read(4)
    if (len(x) == 4):
      sz = struct.unpack('!i', x)[0]
      return self.chan_in.read(sz)
  
  # Send data to Erlang
  def send(self, data):
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

def decode_command(erlport, erlz3, data):
  cmd = json.loads(data)
  tp = cmd["t"]
  opts = {
    cc.CMD_LOAD_TRACE_FILE: decode_load_trace_file,
    cc.CMD_SOLVE: decode_solve,
    cc.CMD_GET_MODEL: decode_get_model,
    cc.CMD_ADD_AXIOMS: decode_add_axioms,
    cc.CMD_FIX_VARIABLE: decode_fix_variable,
    cc.CMD_RESET_SOLVER: decode_reset_solver,
    cc.CMD_STOP: decode_stop,
  }
  opts[tp](erlport, erlz3, cmd)

# Load a trace file
def decode_load_trace_file(erlport, erlz3, cmd):
  r = cIO.JsonReader(cmd["f"], cmd["e"])
  for tp, tag, x, rev in r:
    if cc.is_interpretable(tp):
      erlz3.command_toZ3(tp, x, rev)

# Solve the model
def decode_solve(erlport, erlz3, cmd):
  slv = erlz3.solve()
  erlport.send(str(slv))

# Get the model
def decode_get_model(erlport, erlz3, cmd):
  md = erlz3.model
  enc = erlz3.encode_model()
  erlport.send(cc.RSP_MODEL_DELIMITER_START)
#  erlport.send(str(md))
  for s, v in enc:
    erlport.send(str(json.dumps(s, sort_keys=True)))
    erlport.send(str(json.dumps(v, sort_keys=True)))
  erlport.send(cc.RSP_MODEL_DELIMITER_END)

# Add axioms
def decode_add_axioms(erlport, erlz3, cmd):
  erlz3.add_axioms()

# Fix variable to specific value
def decode_fix_variable(erlport, erlz3, cmd):
  var, val = cmd["s"], cmd["v"]
  erlz3.fix_parameter(var, val)

# Reset the solver
def decode_reset_solver(erlport, erlz3, cmd):
  erlz3.reset_solver()

# Stop the execution
def decode_stop(erlport, erlz3, cmd):
  cglb.__RUN__ = False

