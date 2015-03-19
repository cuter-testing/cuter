#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, json, struct
import cuter_global as cglb
import cuter_common as cc
import cuter_io as cio

scnt = 0
symbs = {}

def pprint(ls):
  print ls[0]
  for l in ls[1:]:
    print "  %s" % l

def pretty_list(l):
  return ", ".join(map(pretty, l))

def pretty(d):
  global scnt, symbs
  try:
    # Symbolic Variable
    if 's' in d:
      s = d['s']
      if s in symbs:
        return symbs[s]
      else:
        scnt += 1
        x = "x%s" % scnt
        symbs[s] = x
        return x
    # Type
    if "tp" in d:
      return pretty_type(d)
    # Int
    if d['t'] == cc.JSON_TYPE_INT:
      return d['v']
    # Atom
    if d['t'] == cc.JSON_TYPE_ATOM:
      return "".join(map(chr, d['v']))
    if d['t'] == cc.JSON_TYPE_LIST:
      return "[%s]" % pretty_list(d['v'])
  except KeyError:
    pass
  return str(d)

def pretty_type(d):
  tp = d["tp"]
  if tp == cc.JSON_ERLTYPE_ANY:
    return "any"
  if tp == cc.JSON_ERLTYPE_ATOM:
    return "atom"
  if tp == cc.JSON_ERLTYPE_ATOMLIT:
    return pretty(d["a"])
  if tp == cc.JSON_ERLTYPE_BOOLEAN:
    return "boolean"
  if tp == cc.JSON_ERLTYPE_FLOAT:
    return "float"
  if tp == cc.JSON_ERLTYPE_INTEGER:
    return "integer"
  if tp == cc.JSON_ERLTYPE_INTEGERLIT:
    return pretty(d["a"])
  if tp == cc.JSON_ERLTYPE_LIST:
    return "[%s]" % pretty_type(d["a"])
  if tp == cc.JSON_ERLTYPE_NIL:
    return "[]"
  if tp == cc.JSON_ERLTYPE_TUPLE:
    return "tuple"
  if tp == cc.JSON_ERLTYPE_TUPLEDET:
    return "{%s}" % ", ".join(map(pretty_type, d["a"]))
  if tp == cc.JSON_ERLTYPE_UNION:
    return "|".join(map(pretty_type, d["a"]))

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
    pprint([
      "PARAMS",
      pretty_list(json_data["a"])
    ])
  # Symbolic parameters
  elif tp == cc.OP_SPEC:
    msg = ["SPEC"]
    for sp in json_data["a"]:
      msg.append("(%s) -> %s" % (pretty_list(sp["p"]), pretty(sp["r"])))
    pprint(msg)
  # True guard constraint
  elif tp == cc.OP_GUARD_TRUE:
    xs = json_data["a"]
    pprint([
      "TRUE GUARD",
      "%s : true" % pretty(xs[0])
    ])
  # False guard constraint
  elif tp == cc.OP_GUARD_FALSE:
    xs = json_data["a"]
    pprint([
      "FALSE GUARD",
      "%s : false" % pretty(xs[0])
    ])
  # Match equal constraint
  elif tp == cc.OP_MATCH_EQUAL_TRUE:
    xs = json_data["a"]
    pprint([
      "MATCH EQUAL TRUE",
      "%s == %s" % (pretty(xs[0]), pretty(xs[1]))
    ])
  # Match not equal constraint
  elif tp == cc.OP_MATCH_EQUAL_FALSE:
    xs = json_data["a"]
    pprint([
      "MATCH EQUAL FALSE",
      "%s =/= %s" % (pretty(xs[0]), pretty(xs[1]))
    ])
  # Tuple of size N constraint
  elif tp == cc.OP_TUPLE_SZ:
    xs = json_data["a"]
    pprint([
      "TUPLE SZ",
      "%s : tuple of size %s" % (pretty(xs[0]), pretty(xs[1]))
    ])
  # Tuple of not size N constraint
  elif tp == cc.OP_TUPLE_NOT_SZ:
    xs = json_data["a"]
    pprint([
      "TUPLE NOT SZ",
      "%s : tuple of not size %s" % (pretty(xs[0]), pretty(xs[1]))
    ])
  # Not a tuple constraint
  elif tp == cc.OP_TUPLE_NOT_TPL:
    xs = json_data["a"]
    pprint([
      "TUPLE NOT TPL",
      "%s : not a tuple (of size %s)" % (pretty(xs[0]), pretty(xs[1]))
    ])
  # Nonempty list constraint
  elif tp == cc.OP_LIST_NON_EMPTY:
    xs = json_data["a"]
    pprint([
      "LIST NONEMPTY",
      "%s : nonempty list" % pretty(xs[0])
    ])
  # Empty list constraint
  elif tp == cc.OP_LIST_EMPTY:
    xs = json_data["a"]
    pprint([
      "LIST EMPTY",
      "%s : empty list" % pretty(xs[0])
    ])
  # Not a list constraint
  elif tp == cc.OP_LIST_NOT_LST:
    xs = json_data["a"]
    pprint([
      "LIST NOT LST",
      "%s : not a list" % pretty(xs[0])
    ])
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
    print "%s =:= { %s }" % (pretty(xs[0]), pretty_list(xs[1:]))
  # Unfold a symbolic list
  elif tp == cc.OP_UNFOLD_LIST:
    print "UNFOLD LIST"
    xs = json_data["a"]
    print "%s =:= [ %s ]" % (pretty(xs[0]), pretty_list(xs[1:]))
  # Get the head of a list
  elif tp == cc.OP_HD:
    xs = json_data["a"]
    pprint([
      "HD",
      "%s := hd( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ])
  # Get the tail of a list
  elif tp == cc.OP_TL:
    xs = json_data["a"]
    pprint([
      "TL",
      "%s := tl( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ])
  # Is a term an integer
  elif tp == cc.OP_IS_INTEGER:
    print "IS INTEGER"
    xs = json_data["a"]
    print "%s = is_integer( %s )" % (pretty(xs[0]), pretty(xs[1]))
  # Is term an atom
  elif tp == cc.OP_IS_ATOM:
    print "IS ATOM"
    xs = json_data["a"]
    print "%s = is_atom( %s )" % (pretty(xs[0]), pretty(xs[1]))
  # Is term a float
  elif tp == cc.OP_IS_FLOAT:
    print "IS FLOAT"
    xs = json_data["a"]
    print "%s = is_float( %s )" % (pretty(xs[0]), pretty(xs[1]))
  # Is term a list
  elif tp == cc.OP_IS_LIST:
    print "IS LIST"
    xs = json_data["a"]
    print "%s = is_list( %s )" % (pretty(xs[0]), pretty(xs[1]))
  # Is term a tuple
  elif tp == cc.OP_IS_TUPLE:
    print "IS TUPLE"
    xs = json_data["a"]
    print "%s = is_tuple( %s )" % (pretty(xs[0]), pretty(xs[1]))
  # Is term a boolean
  elif tp == cc.OP_IS_BOOLEAN:
    print "IS BOOLEAN"
    xs = json_data["a"]
    print "%s = is_boolean( %s )" % (pretty(xs[0]), pretty(xs[1]))
  # Is term a number
  elif tp == cc.OP_IS_NUMBER:
    print "IS NUMBER"
    xs = json_data["a"]
    print "%s = is_number( %s )" % (pretty(xs[0]), pretty(xs[1]))
  # Add two numbers
  elif tp == cc.OP_PLUS:
    print "PLUS"
    xs = json_data["a"]
    print "%s = %s + %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
  # Subtract two numbers
  elif tp == cc.OP_MINUS:
    print "MINUS"
    xs = json_data["a"]
    print "%s = %s - %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
  # Multiply two numbers
  elif tp == cc.OP_TIMES:
    print "TIMES"
    xs = json_data["a"]
    print "%s = %s * %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
  # Divide two numbers
  elif tp == cc.OP_RDIV:
    print "REAL DIVISION"
    xs = json_data["a"]
    print "%s = %s / %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
  # Integer division of natural numbers
  elif tp == cc.OP_IDIV_NAT:
    print "INTEGER DIVISION OF NAT"
    xs = json_data["a"]
    print "%s = %s div %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
  # Remainder of integer division of natural numbers
  elif tp == cc.OP_REM_NAT:
    print "REMAINDER OF INTEGER DIVISION OF NAT"
    xs = json_data["a"]
    print "%s = %s rem %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
  # Unary operation
  elif tp == cc.OP_UNARY:
    print "UNARY"
    xs = json_data["a"]
    print "%s = - %s" % (pretty(xs[0]), pretty(xs[1]))
  # Equality of terms
  elif tp == cc.OP_EQUAL:
    xs = json_data["a"]
    pprint([
      "EQUAL",
      "%s := (%s == %s)" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ])
  # Inequality of terms
  elif tp == cc.OP_UNEQUAL:
    print "UNEQUAL"
    xs = json_data["a"]
    print "%s = %s =/= %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
  # Convert a number to float
  elif tp == cc.OP_FLOAT:
    print "TO FLOAT"
    xs = json_data["a"]
    print "%s = float( %s )" % (pretty(xs[0]), pretty(xs[1]))
  else:
    print "UNKNOWN OPCODE", tp
    xs = json_data["a"]
    print xs
    sys.exit(1)

print "Total Commands:", n

