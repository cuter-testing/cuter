#!/usr/bin/env python
# -*- coding: utf-8 -*-

import traceback
import cuter_global as cglb
import cuter_port as cp
import cuter_io as cIO
import cuter_z3 as cz3
import cuter_common as cc


## Main Program

try:
  cglb.init()
  
  erlport = cp.ErlangPort() # Initialize the communication with Erlang
  erlz3 = cz3.ErlangZ3()    # Initialize the Z3 interface

  while cglb.__RUN__:
    data = erlport.receive()
    erlport.send(str(data)) # Just print back the command
    cmd = cp.decode_command(erlport, erlz3, data)

except:
  e = traceback.format_exc()
  erlport.send(e)
