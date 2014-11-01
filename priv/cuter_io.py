#!/usr/bin/env python
# -*- coding: utf-8 -*-

import gzip, json, struct, sys
import cuter_global as cglb
import cuter_common as cc

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
  
  def read(self, sz):
    return self.fd.read(sz)
  
  # Decode 4 bytes that represent the size of the entry
  def size(self):
    x = [struct.unpack('B', self.read(1))[0] for z in range(4)]
    return (x[0] << 24) | (x[1] << 16) | (x[2] << 8) | x[3]
  
  # Decode 1 byte that represents the kind of the entry
  def kind(self):
    x = self.read(1)
    if (x == ""):
      raise BinaryEOF
    else:
      return struct.unpack('B', x)[0]
  
  # Decode 1 byte that represents the type of the entry
  def entry_type(self):
    x = self.read(1)
    if (x == ""):
      raise BinaryEOF
    else:
      return struct.unpack('B', x)[0]
  
  def __iter__(self):
    return self
  
  def next(self):
    if self.cnt == self.end:
      raise StopIteration
    try:
      k = self.kind()
      tp = self.entry_type()
      if cglb.__TTY__:
        print "\n", tp
      sz = self.size()
      data = self.read(sz)
      rev = False
      if (cc.is_reversible(k, tp)):
        self.cnt += 1
        if cglb.__TTY__:
          print "RVS -- %s" % self.cnt
        if self.cnt == self.end:
          rev = True
      
      json_data = json.loads(data)
      return tp, json_data, rev
    except BinaryEOF:
      raise StopIteration
  
  def __del__(self):
    self.fd.close()
