import gzip, json, struct, sys

class ErlangPort:
  def __init__(self):
    self.chan_in = sys.stdin
    self.chan_out = sys.stdout
  
  def receive(self):
    x = self.chan_in.read(2)
    if (len(x) == 2):
      sz = struct.unpack('!h', x)[0]
      return self.chan_in.read(sz)
  
  def send(self, data):
    sz = len(data)
    x = struct.pack('!h', sz)
    self.chan_out.write(x)
    return self.chan_out.write(data)

class PortCommand:
  def __init__(self, port_data):
    cmd = json.loads(port_data)
    self.decode_command(cmd)

  def decode_command(self, cmd):
    self.type = cmd["t"]
    if ("a" in cmd):
      self.args = tuple(cmd["a"])


class BinaryEOF(Exception):
  def __init__(self):
    pass
  def __str__(self):
    return 'EOF encountered'

class JsonReader:
  def __init__(self, filename, end):
    self.fd = gzip.open(filename, 'rb')
    self.cnt = 0
    self.end = end
  
  def size(self):
    x = [struct.unpack('B', self.fd.read(1))[0] for z in range(4)]
    return (x[0] << 24) | (x[1] << 16) | (x[2] << 8) | x[3]
  
  def reverse_constraint(self, json_data):
    json_data["r"] = True
    return json_data
  
  def kind(self):
    x = self.fd.read(1)
    if (x == ""):
      raise BinaryEOF
    else:
      return struct.unpack('B', x)[0]
  
  def read(self, sz):
    return self.fd.read(sz)
  
  def __iter__(self):
    return self
  
  def next(self):
    if (self.cnt == self.end):
      raise StopIteration
    try:
      k = self.kind()
      if (self.is_constraint(k)):
        self.cnt += 1
      sz = self.size()
      data = self.read(sz)
      json_data = json.loads(data)
      if (self.cnt == self.end):
        return self.reverse_constraint(json_data)
      else:
        return json_data
    except BinaryEOF:
      raise StopIteration
  
  def is_constraint(self, t):
    if (t == 1 or t == 2):
      return True
    else:
      return False
  
  def __del__(self):
    self.fd.close()

class JsonWriter:
  def __init__(self, filename):
    self.fd = open(filename, 'wb')
  
  def write(self, data):
    json_data = json.dumps(data, separators=(',', ':'))
    self.fd.write(json_data)
  
  def __del__(self):
    self.fd.close()
  
