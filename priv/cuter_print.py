#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import json
import struct
import cuter_global as cglb
import cuter_common as cc
import cuter_io as cio

if "tty" in sys.argv:
  sys.argv.remove("tty")
cglb.init()

fname = sys.argv[1]
n = 0
for tp, json_data, rev in cio.JsonReader(fname, 100000000):
  n += 1
#  print tp
#  print json_data
  
  # Symbolic parameters
  if tp == cc.OP_PARAMS:
    print "PARAMS"
    print [x["s"] for x in json_data["a"]]
  # True guard constraint
  elif tp == cc.OP_GUARD_TRUE:
    print "TRUE GUARD"
    xs = json_data["a"]
    print "%s is true" % xs[0]
  # False guard constraint
  elif tp == cc.OP_GUARD_FALSE:
    print "FALSE GUARD"
    xs = json_data["a"]
    print "%s is false" % xs[0]
  # Match equal constraint
  elif tp == cc.OP_MATCH_EQUAL_TRUE:
    print "MATCH EQUAL TRUE"
    xs = json_data["a"]
    print "%s =:= %s" % (xs[0], xs[1])
  # Match not equal constraint
  elif tp == cc.OP_MATCH_EQUAL_FALSE:
    print "MATCH EQUAL FALSE"
    xs = json_data["a"]
    print "%s =/= %s" % (xs[0], xs[1])
  # Tuple of size N constraint
  elif tp == cc.OP_TUPLE_SZ:
    print "TUPLE SZ"
    xs = json_data["a"]
    print "%s is a tuple of size %s" % (xs[0], xs[1])
  # Tuple of not size N constraint
  elif tp == cc.OP_TUPLE_NOT_SZ:
    print "TUPLE NOT SZ"
    xs = json_data["a"]
    print "%s is a tuple of not size %s" % (xs[0], xs[1])
  # Not a tuple constraint
  elif tp == cc.OP_TUPLE_NOT_TPL:
    print "TUPLE NOT TPL"
    xs = json_data["a"]
    print "%s is not a tuple (of size %s)" % (xs[0], xs[1])
  # Nonempty list constraint
  elif tp == cc.OP_LIST_NON_EMPTY:
    print "LIST NONEMPTY"
    xs = json_data["a"]
    print "%s is a nonempty list" % xs[0]
  # Empty list constraint
  elif tp == cc.OP_LIST_EMPTY:
    print "LIST EMPTY"
    xs = json_data["a"]
    print "%s is an empty list" % xs[0]
  # Not a list constraint
  elif tp == cc.OP_LIST_NOT_LST:
    print "LIST NOT LST"
    xs = json_data["a"]
    print "%s is not a list" % xs[0]
  # Spawn a process
  elif tp == cc.OP_SPAWN:
    print "SPAWN"
    xs = json_data["a"]
    print "spawn child %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
  # Spawned by a process
  elif tp == cc.OP_SPAWNED:
    print "SPAWNED"
    xs = json_data["a"]
    print "spawned by %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
  # Send a message
  elif tp == cc.OP_MSG_SEND:
    print "MSG SEND"
    xs = json_data["a"]
    print "send msg to %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
  # Receive a message
  elif tp == cc.OP_MSG_RECEIVE:
    print "MSG RECEIVE"
    xs = json_data["a"]
    print "receive msg from %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
  # Consume a message
  elif tp == cc.OP_MSG_CONSUME:
    print "MSG CONSUME"
    xs = json_data["a"]
    print "consume msg from %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
  # Unfold a symbolic tuple
  elif tp == cc.OP_UNFOLD_TUPLE:
    print "UNFOLD TUPLE"
    xs = json_data["a"]
    print "%s =:= { %s }" % (xs[0], xs[1:])
  # Unfold a symbolic list
  elif tp == cc.OP_UNFOLD_LIST:
    print "UNFOLD LIST"
    xs = json_data["a"]
    print "%s =:= [ %s ]" % (xs[0], xs[1:])
  # Get the head of a list
  elif tp == cc.OP_HD:
    print "HD"
    xs = json_data["a"]
    print "%s = hd( %s )" % (xs[0], xs[1])
  # Get the tail of a list
  elif tp == cc.OP_TL:
    print "TL"
    xs = json_data["a"]
    print "%s = tl( %s )" % (xs[0], xs[1])
  # Is a term an integer
  elif tp == cc.OP_IS_INTEGER:
    print "IS INTEGER"
    xs = json_data["a"]
    print "%s = is_integer( %s )" % (xs[0], xs[1])
  # Is term an atom
  elif tp == cc.OP_IS_ATOM:
    print "IS ATOM"
    xs = json_data["a"]
    print "%s = is_atom( %s )" % (xs[0], xs[1])
  # Is term a float
  elif tp == cc.OP_IS_FLOAT:
    print "IS FLOAT"
    xs = json_data["a"]
    print "%s = is_float( %s )" % (xs[0], xs[1])
  # Is term a list
  elif tp == cc.OP_IS_LIST:
    print "IS LIST"
    xs = json_data["a"]
    print "%s = is_list( %s )" % (xs[0], xs[1])
  # Is term a tuple
  elif tp == cc.OP_IS_TUPLE:
    print "IS TUPLE"
    xs = json_data["a"]
    print "%s = is_tuple( %s )" % (xs[0], xs[1])
  # Is term a boolean
  elif tp == cc.cc.OP_IS_BOOLEAN:
    print "IS BOOLEAN"
    xs = json_data["a"]
    print "%s = is_boolean( %s )" % (xs[0], xs[1])
  # Is term a number
  elif tp == cc.OP_IS_NUMBER:
    print "IS NUMBER"
    xs = json_data["a"]
    print "%s = is_number( %s )" % (xs[0], xs[1])
  # Add two numbers
  elif tp == cc.OP_PLUS:
    print "PLUS"
    xs = json_data["a"]
    print "%s = %s + %s" % (xs[0], xs[1], xs[2])
  # Subtract two numbers
  elif tp == cc.OP_MINUS:
    print "MINUS"
    xs = json_data["a"]
    print "%s = %s - %s" % (xs[0], xs[1], xs[2])
  # Multiply two numbers
  elif tp == cc.OP_TIMES:
    print "TIMES"
    xs = json_data["a"]
    print "%s = %s * %s" % (xs[0], xs[1], xs[2])
  # Divide two numbers
  elif tp == cc.OP_RDIV:
    print "REAL DIVISION"
    xs = json_data["a"]
    print "%s = %s / %s" % (xs[0], xs[1], xs[2])
  # Integer division of natural numbers
  elif tp == cc.OP_IDIV_NAT:
    print "INTEGER DIVISION OF NAT"
    xs = json_data["a"]
    print "%s = %s div %s" % (xs[0], xs[1], xs[2])
  # Remainder of integer division of natural numbers
  elif tp == cc.OP_REM_NAT:
    print "REMAINDER OF INTEGER DIVISION OF NAT"
    xs = json_data["a"]
    print "%s = %s rem %s" % (xs[0], xs[1], xs[2])
  # Unary operation
  elif tp == cc.OP_UNARY:
    print "UNARY"
    xs = json_data["a"]
    print "%s = - %s" % (xs[0], xs[1])

print "Total Commands:", n

