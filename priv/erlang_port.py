import json, traceback
from json_utils import *
from z3_utils import *

## Main Program

try:
  erlz3 = ErlangZ3()
  erlport = ErlangPort()
  
  wait = True
  while wait:
    data = erlport.receive()
    cmd = PortCommand(data)
    
    if cmd.type == "load":
      f, start, end = cmd.args
      for c in JsonReader(f, end):
        erlz3.json_command_to_z3(c)
      
    elif cmd.type == "check":
      wait = False
  
  chk = erlz3.solve()
  erlport.send(str(erlz3.check))
  
  if chk:
    data = erlport.receive()
    cmd = PortCommand(data)
    if cmd.type == "model":
      sol = erlz3.z3_solution_to_json()
      erlport.send(str(json.dumps(sol)))
    
    yy = JsonWriter("sol")
    yy.write(sol)
except:
  e = traceback.format_exc()
  erlport.send(e)


