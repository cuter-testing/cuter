#!/usr/bin/env python
# -*- coding: utf-8 -*-

import traceback
import cuter_global as cglb
import cuter_port as cp
import cuter_io as cIO
import cuter_z3 as cz3
import cuter_logger as clg
import cuter_common as cc

## Main Program

cglb.init()
erlport = cp.ErlangPort() # Initialize the communication with Erlang
erlz3 = cz3.ErlangZ3()    # Initialize the Z3 interface

try:
  while cglb.__RUN__:
    data = erlport.receive()
    clg.data_received(data)
    cmd = cp.decode_command(erlport, erlz3, data)
  clg.clean_empty_logs()

except:
  e = traceback.format_exc()
  erlport.send(e)
