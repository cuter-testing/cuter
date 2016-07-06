#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, json, struct
import cuter_global as cglb
import cuter_common as cc
import cuter_io as cio

with_tags = True if "tags" in sys.argv else False

scnt = 0
symbs = {}

def pprint(ls, tag):
  if with_tags:
    print "%s (%s)" % (ls[0], tag)
  else:
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
        return symbs[s] + "("+s+")"
      else:
        scnt += 1
        x = "x%s" % scnt
        symbs[s] = x
        return x + "("+s+")"
    # Type
    if "tp" in d:
      return pretty_type(d)
    # Int
    if d['t'] == cc.JSON_TYPE_INT:
      return str(d['v'])
    # Float
    if d['t'] == cc.JSON_TYPE_FLOAT:
      return str(d['v'])
    # Atom
    if d['t'] == cc.JSON_TYPE_ATOM:
      return "".join(map(chr, d['v']))
    # List
    if d['t'] == cc.JSON_TYPE_LIST:
      return "[%s]" % pretty_list(d['v'])
    # Bitstring
    if d['t'] == cc.JSON_TYPE_BITSTRING:
      return "<<%s>>" % pretty_list(d['v'])
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
  if tp == cc.JSON_ERLTYPE_GENERIC_FUN:
    return "fun()"
  if tp == cc.JSON_ERLTYPE_FUN:
    args, ret = d["a"][:-1], d["a"][-1]
    return "fun(({}) -> {})".format(", ".join(map(pretty_type, args)), pretty_type(ret))

def print_cmd(tp, tag, json_data, rev):
  # Symbolic parameters
  if tp == cc.OP_PARAMS:
    pprint([
      "PARAMS",
      pretty_list(json_data["a"])
    ], tag)
  # Symbolic parameters
  elif tp == cc.OP_SPEC:
    msg = ["SPEC"]
    try:
      for sp in json_data["a"]:
        msg.append("(%s) -> %s" % (pretty_list(sp["p"]), pretty(sp["r"])))
    except:
      msg.append("Failed to parse the spec.")
    pprint(msg, tag)
  # True guard constraint
  elif tp == cc.OP_GUARD_TRUE:
    xs = json_data["a"]
    pprint([
      "TRUE GUARD",
      "%s : true" % pretty(xs[0])
    ], tag)
  # False guard constraint
  elif tp == cc.OP_GUARD_FALSE:
    xs = json_data["a"]
    pprint([
      "FALSE GUARD",
      "%s : false" % pretty(xs[0])
    ], tag)
  # Match equal constraint
  elif tp == cc.OP_MATCH_EQUAL_TRUE:
    xs = json_data["a"]
    pprint([
      "MATCH EQUAL TRUE",
      "%s == %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Match not equal constraint
  elif tp == cc.OP_MATCH_EQUAL_FALSE:
    xs = json_data["a"]
    pprint([
      "MATCH EQUAL FALSE",
      "%s =/= %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Tuple of size N constraint
  elif tp == cc.OP_TUPLE_SZ:
    xs = json_data["a"]
    pprint([
      "TUPLE SZ",
      "%s : tuple of size %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Tuple of not size N constraint
  elif tp == cc.OP_TUPLE_NOT_SZ:
    xs = json_data["a"]
    pprint([
      "TUPLE NOT SZ",
      "%s : tuple of not size %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Not a tuple constraint
  elif tp == cc.OP_TUPLE_NOT_TPL:
    xs = json_data["a"]
    pprint([
      "TUPLE NOT TPL",
      "%s : not a tuple (of size %s)" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Nonempty list constraint
  elif tp == cc.OP_LIST_NON_EMPTY:
    xs = json_data["a"]
    pprint([
      "LIST NONEMPTY",
      "%s : nonempty list" % pretty(xs[0])
    ], tag)
  # Empty list constraint
  elif tp == cc.OP_LIST_EMPTY:
    xs = json_data["a"]
    pprint([
      "LIST EMPTY",
      "%s : empty list" % pretty(xs[0])
    ], tag)
  # Not a list constraint
  elif tp == cc.OP_LIST_NOT_LST:
    xs = json_data["a"]
    pprint([
      "LIST NOT LST",
      "%s : not a list" % pretty(xs[0])
    ], tag)
  # Spawn a process
  elif tp == cc.OP_SPAWN:
    xs = json_data["a"]
    pprint([
      "SPAWN",
      "spawn child %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Spawned by a process
  elif tp == cc.OP_SPAWNED:
    xs = json_data["a"]
    pprint([
      "SPAWNED",
      "spawned by %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Send a message
  elif tp == cc.OP_MSG_SEND:
    xs = json_data["a"]
    pprint([
      "MSG SEND",
      "send msg to %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Receive a message
  elif tp == cc.OP_MSG_RECEIVE:
    xs = json_data["a"]
    pprint([
      "MSG RECEIVE",
      "receive msg from %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Consume a message
  elif tp == cc.OP_MSG_CONSUME:
    xs = json_data["a"]
    pprint([
      "MSG CONSUME",
      "consume msg from %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Unfold a symbolic tuple
  elif tp == cc.OP_UNFOLD_TUPLE:
    xs = json_data["a"]
    pprint([
      "UNFOLD TUPLE",
      "%s =:= { %s }" % (pretty(xs[0]), pretty_list(xs[1:]))
    ], tag)
  # Unfold a symbolic list
  elif tp == cc.OP_UNFOLD_LIST:
    xs = json_data["a"]
    pprint([
      "UNFOLD LIST",
      "%s =:= [ %s ]" % (pretty(xs[0]), pretty_list(xs[1:]))
    ], tag)
  # Get the head of a list
  elif tp == cc.OP_HD:
    xs = json_data["a"]
    pprint([
      "HD",
      "%s := hd( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Get the tail of a list
  elif tp == cc.OP_TL:
    xs = json_data["a"]
    pprint([
      "TL",
      "%s := tl( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is a term an integer
  elif tp == cc.OP_IS_INTEGER:
    xs = json_data["a"]
    pprint([
      "IS INTEGER",
      "%s = is_integer( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term an atom
  elif tp == cc.OP_IS_ATOM:
    xs = json_data["a"]
    pprint([
      "IS ATOM",
      "%s = is_atom( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a float
  elif tp == cc.OP_IS_FLOAT:
    xs = json_data["a"]
    pprint([
      "IS FLOAT",
      "%s = is_float( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a list
  elif tp == cc.OP_IS_LIST:
    xs = json_data["a"]
    pprint([
      "IS LIST",
      "%s = is_list( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a tuple
  elif tp == cc.OP_IS_TUPLE:
    xs = json_data["a"]
    pprint([
      "IS TUPLE",
      "%s = is_tuple( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a boolean
  elif tp == cc.OP_IS_BOOLEAN:
    xs = json_data["a"]
    pprint([
      "IS BOOLEAN",
      "%s = is_boolean( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a number
  elif tp == cc.OP_IS_NUMBER:
    xs = json_data["a"]
    pprint([
      "IS NUMBER",
      "%s = is_number( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Add two numbers
  elif tp == cc.OP_PLUS:
    xs = json_data["a"]
    pprint([
      "PLUS",
      "%s = %s + %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Subtract two numbers
  elif tp == cc.OP_MINUS:
    xs = json_data["a"]
    pprint([
      "MINUS",
      "%s = %s - %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Multiply two numbers
  elif tp == cc.OP_TIMES:
    xs = json_data["a"]
    pprint([
      "TIMES",
      "%s = %s * %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Divide two numbers
  elif tp == cc.OP_RDIV:
    xs = json_data["a"]
    pprint([
      "REAL DIVISION",
      "%s = %s / %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Integer division of natural numbers
  elif tp == cc.OP_IDIV_NAT:
    xs = json_data["a"]
    pprint([
      "INTEGER DIVISION OF NAT",
      "%s = %s div %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Remainder of integer division of natural numbers
  elif tp == cc.OP_REM_NAT:
    xs = json_data["a"]
    pprint([
      "REMAINDER OF INTEGER DIVISION OF NAT",
      "%s = %s rem %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Unary operation
  elif tp == cc.OP_UNARY:
    xs = json_data["a"]
    pprint([
      "UNARY",
      "%s = - %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Equality of terms
  elif tp == cc.OP_EQUAL:
    xs = json_data["a"]
    pprint([
      "EQUAL",
      "%s := (%s == %s)" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Inequality of terms
  elif tp == cc.OP_UNEQUAL:
    xs = json_data["a"]
    pprint([
      "UNEQUAL",
      "%s = %s =/= %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Convert a number to float
  elif tp == cc.OP_FLOAT:
    xs = json_data["a"]
    pprint([
      "TO FLOAT",
      "%s = float( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Power
  elif tp == cc.OP_POW:
    xs = json_data["a"]
    pprint([
      "POW",
      "%s = %s ** %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Bitmatch const true
  elif tp == cc.OP_BITMATCH_CONST_TRUE:
    xs = json_data["a"]
    pprint([
      "BITMATCH CONST TRUE",
      "%s:%s ++ %s = %s " % (pretty(xs[1]), pretty(xs[2]), pretty(xs[0]), pretty(xs[3]))
    ], tag)
  # Bitmatch const false
  elif tp == cc.OP_BITMATCH_CONST_FALSE:
    xs = json_data["a"]
    pprint([
      "BITMATCH CONST FALSE",
      "%s:%s is not head of %s " % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Bitmatch var true
  elif tp == cc.OP_BITMATCH_VAR_TRUE:
    xs = json_data["a"]
    pprint([
      "BITMATCH VAR TRUE",
      "%s with size %s + %s = %s " % (pretty(xs[0]), pretty(xs[2]), pretty(xs[1]), pretty(xs[3]))
    ], tag)
  # Tcons
  elif tp == cc.OP_TCONS:
    xs = json_data["a"]
    pprint([
      "TCONS",
      "%s = [ %s ] " % (pretty(xs[0]), pretty_list(xs[1:]))
    ], tag)
  # Less than (with integers)
  elif tp == cc.OP_LT_INT:
    xs = json_data["a"]
    pprint([
      "LT INT",
      "%s = %s < %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Bitmach var false
  elif tp == cc.OP_BITMATCH_VAR_FALSE:
    xs = json_data["a"]
    pprint([
      "BITMATCH VAR FALSE",
      "%s with size %s" % (pretty(xs[1]), pretty(xs[0]))
    ], tag)
  # Cons
  elif tp == cc.OP_CONS:
    xs = json_data["a"]
    pprint([
      "CONS",
      "%s = [ %s | %s ] " % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Fun App
  elif tp == cc.OP_LAMBDA:
    xs = json_data["a"]
    pprint([
      "LAMBDA",
      "%s = apply(%s, [ %s ]) " % (pretty(xs[0]), pretty(xs[1]), pretty_list(xs[2:]))
    ], tag)
  # Is Function
  elif tp == cc.OP_IS_FUN_WITH_ARITY:
    xs = json_data["a"]
    pprint([
      "IS FUNCTION WITH ARITY",
      "%s = is_function(%s, %s) " % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Fresh lambda with arity
  elif tp == cc.OP_FRESH_LAMBDA_WITH_ARITY:
    xs = json_data["a"]
    pprint([
      "FRESH LAMBDA WITH ARITY",
      "is_function(%s, %s) " % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Evaluated Closure
  elif tp == cc.OP_EVALUATED_CLOSURE:
    xs = json_data["a"]
    pprint([
      "EVALUATED CLOSURE",
      "%s = apply(%s, [ %s ]) " % (pretty(xs[0]), pretty(xs[1]), pretty_list(xs[2:]))
    ], tag)
  else:
    print "UNKNOWN OPCODE", tp
    xs = json_data["a"]
    print xs
    sys.exit(1)

if __name__ == "__main__":
  if "tty" in sys.argv:
    sys.argv.remove("tty")
  cglb.init()
  fname = sys.argv[1]
  n = 0
  for tp, tag, json_data, rev in cio.JsonReader(fname, 100000000):
    n += 1
    print_cmd(tp, tag, json_data, rev)
  print "Total Commands:", n
