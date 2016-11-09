#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, json, struct
import cuter_global as cglb
import cuter_common as cc
import cuter_io as cio
from cuter_proto_log_entry_pb2 import LogEntry

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
    if cc.is_symb(d):
      s = cc.get_symb(d)
      if s in symbs:
        return symbs[s] + "("+s+")"
      else:
        scnt += 1
        x = "x%s" % scnt
        symbs[s] = x
        return x + "("+s+")"
    # Type
    if cc.is_type_message(d):
      return pretty_type(d)
    # Int
    if cc.is_int(d):
      return str(cc.get_int(d))
    # Float
    if cc.is_float(d):
      return str(cc.get_float(d))
    # Atom
    if cc.is_atom(d):
      cs = cc.get_atom_chars(d)
      return "".join(map(chr, cs))
    # List
    if cc.is_list(d):
      return "[%s]" % pretty_list(cc.get_list_subterms(d))
    # Bitstring
    if cc.is_bitstring(d):
      bits = map(lambda x: 1 if x else 0, cc.get_bits(d))
      return "<<%s>>" % pretty_list(bits)
  except KeyError:
    pass
  return str(d)

def pretty_type(d):
    if cc.is_type_any(d):
      return "any"
    if cc.is_type_atom(d):
      return "atom"
    if cc.is_type_atomlit(d):
      return pretty(cc.get_literal_from_atomlit(d))
    if cc.is_type_float(d):
      return "float"
    if cc.is_type_integer(d):
      return "integer"
    if cc.is_type_integerlit(d):
      return pretty(cc.get_literal_from_integerlit(d))
    if cc.is_type_list(d):
      return "[%s]" % pretty_type(cc.get_inner_type_from_list(d))
    if cc.is_type_nil(d):
      return "[]"
    if cc.is_type_tuple(d):
      return "tuple"
    if cc.is_type_tupledet(d):
      return "{%s}" % ", ".join(map(pretty_type, cc.get_inner_types_from_tupledet(d)))
    if cc.is_type_union(d):
      return "|".join(map(pretty_type, cc.get_inner_types_from_union(d)))
    if cc.is_type_range(d):
      bs = cc.get_range_bounds_from_range(d)
      return "%s..%s" % (cc.get_lower_bound(bs), cc.get_upper_bound(bs))
    if cc.is_type_generic_fun(d):
      return "fun()"
    if cc.is_type_complete_fun(d):
      args, ret = d["a"][:-1], d["a"][-1]
      args = cc.get_parameters_from_complete_fun(d)
      ret = cc.get_rettype_from_fun(d)
      return "fun(({}) -> {})".format(", ".join(map(pretty_type, args)), pretty_type(ret))

def print_cmd(entry, rev):
  tp = entry.type
  tag = cc.get_tag(entry)
  # Symbolic parameters
  if tp == LogEntry.OP_PARAMS:
      pprint([
          "PARAMS",
          pretty_list(entry.arguments)
      ], tag)
  # Symbolic parameters
  elif tp == LogEntry.OP_SPEC:
      msg = ["SPEC"]
      for sp in cc.get_spec_clauses(cc.get_spec(entry)):
          pms = cc.get_parameters_from_complete_funsig(sp)
          ret = cc.get_rettype_from_funsig(sp)
          msg.append("(%s) -> %s" % (pretty_list(pms), pretty(ret)))
      pprint(msg, tag)
  # True guard constraint
  elif tp == LogEntry.OP_GUARD_TRUE:
    xs = entry.arguments
    pprint([
      "TRUE GUARD",
      "%s : true" % pretty(xs[0])
    ], tag)
  # False guard constraint
  elif tp == LogEntry.OP_GUARD_FALSE:
    xs = entry.arguments
    pprint([
      "FALSE GUARD",
      "%s : false" % pretty(xs[0])
    ], tag)
  # Match equal constraint
  elif tp == LogEntry.OP_MATCH_EQUAL_TRUE:
    xs = entry.arguments
    pprint([
      "MATCH EQUAL TRUE",
      "%s == %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Match not equal constraint
  elif tp == LogEntry.OP_MATCH_EQUAL_FALSE:
    xs = entry.arguments
    pprint([
      "MATCH EQUAL FALSE",
      "%s =/= %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Tuple of size N constraint
  elif tp == LogEntry.OP_TUPLE_SZ:
    xs = entry.arguments
    pprint([
      "TUPLE SZ",
      "%s : tuple of size %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Tuple of not size N constraint
  elif tp == LogEntry.OP_TUPLE_NOT_SZ:
    xs = entry.arguments
    pprint([
      "TUPLE NOT SZ",
      "%s : tuple of not size %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Not a tuple constraint
  elif tp == LogEntry.OP_TUPLE_NOT_TPL:
    xs = entry.arguments
    pprint([
      "TUPLE NOT TPL",
      "%s : not a tuple (of size %s)" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Nonempty list constraint
  elif tp == LogEntry.OP_LIST_NON_EMPTY:
    xs = entry.arguments
    pprint([
      "LIST NONEMPTY",
      "%s : nonempty list" % pretty(xs[0])
    ], tag)
  # Empty list constraint
  elif tp == LogEntry.OP_LIST_EMPTY:
    xs = entry.arguments
    pprint([
      "LIST EMPTY",
      "%s : empty list" % pretty(xs[0])
    ], tag)
  # Not a list constraint
  elif tp == LogEntry.OP_LIST_NOT_LST:
    xs = entry.arguments
    pprint([
      "LIST NOT LST",
      "%s : not a list" % pretty(xs[0])
    ], tag)
  # Spawn a process
  elif tp == LogEntry.OP_SPAWN:
    xs = entry.arguments
    pprint([
      "SPAWN",
      "spawn child %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Spawned by a process
  elif tp == LogEntry.OP_SPAWNED:
    xs = entry.arguments
    pprint([
      "SPAWNED",
      "spawned by %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Send a message
  elif tp == LogEntry.OP_MSG_SEND:
    xs = entry.arguments
    pprint([
      "MSG SEND",
      "send msg to %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Receive a message
  elif tp == LogEntry.OP_MSG_RECEIVE:
    xs = entry.arguments
    pprint([
      "MSG RECEIVE",
      "receive msg from %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Consume a message
  elif tp == LogEntry.OP_MSG_CONSUME:
    xs = entry.arguments
    pprint([
      "MSG CONSUME",
      "consume msg from %s at node %s with ref %s" % (xs[1]["v"], "".join(chr(i) for i in xs[0]["v"]), xs[2]["v"])
    ], tag)
  # Unfold a symbolic tuple
  elif tp == LogEntry.OP_UNFOLD_TUPLE:
    xs = entry.arguments
    pprint([
      "UNFOLD TUPLE",
      "%s =:= { %s }" % (pretty(xs[0]), pretty_list(xs[1:]))
    ], tag)
  # Unfold a symbolic list
  elif tp == LogEntry.OP_UNFOLD_LIST:
    xs = entry.arguments
    pprint([
      "UNFOLD LIST",
      "%s =:= [ %s ]" % (pretty(xs[0]), pretty_list(xs[1:]))
    ], tag)
  # Get the head of a list
  elif tp == LogEntry.OP_HD:
    xs = entry.arguments
    pprint([
      "HD",
      "%s := hd( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Get the tail of a list
  elif tp == LogEntry.OP_TL:
    xs = entry.arguments
    pprint([
      "TL",
      "%s := tl( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is a term an integer
  elif tp == LogEntry.OP_IS_INTEGER:
    xs = entry.arguments
    pprint([
      "IS INTEGER",
      "%s = is_integer( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term an atom
  elif tp == LogEntry.OP_IS_ATOM:
    xs = entry.arguments
    pprint([
      "IS ATOM",
      "%s = is_atom( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a float
  elif tp == LogEntry.OP_IS_FLOAT:
    xs = entry.arguments
    pprint([
      "IS FLOAT",
      "%s = is_float( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a list
  elif tp == LogEntry.OP_IS_LIST:
    xs = entry.arguments
    pprint([
      "IS LIST",
      "%s = is_list( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a tuple
  elif tp == LogEntry.OP_IS_TUPLE:
    xs = entry.arguments
    pprint([
      "IS TUPLE",
      "%s = is_tuple( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a boolean
  elif tp == LogEntry.OP_IS_BOOLEAN:
    xs = entry.arguments
    pprint([
      "IS BOOLEAN",
      "%s = is_boolean( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Is term a number
  elif tp == LogEntry.OP_IS_NUMBER:
    xs = entry.arguments
    pprint([
      "IS NUMBER",
      "%s = is_number( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Add two numbers
  elif tp == LogEntry.OP_PLUS:
    xs = entry.arguments
    pprint([
      "PLUS",
      "%s = %s + %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Subtract two numbers
  elif tp == LogEntry.OP_MINUS:
    xs = entry.arguments
    pprint([
      "MINUS",
      "%s = %s - %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Multiply two numbers
  elif tp == LogEntry.OP_TIMES:
    xs = entry.arguments
    pprint([
      "TIMES",
      "%s = %s * %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Divide two numbers
  elif tp == LogEntry.OP_RDIV:
    xs = entry.arguments
    pprint([
      "REAL DIVISION",
      "%s = %s / %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Integer division of natural numbers
  elif tp == LogEntry.OP_IDIV_NAT:
    xs = entry.arguments
    pprint([
      "INTEGER DIVISION OF NAT",
      "%s = %s div %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Remainder of integer division of natural numbers
  elif tp == LogEntry.OP_REM_NAT:
    xs = entry.arguments
    pprint([
      "REMAINDER OF INTEGER DIVISION OF NAT",
      "%s = %s rem %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Unary operation
  elif tp == LogEntry.OP_UNARY:
    xs = entry.arguments
    pprint([
      "UNARY",
      "%s = - %s" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Equality of terms
  elif tp == LogEntry.OP_EQUAL:
    xs = entry.arguments
    pprint([
      "EQUAL",
      "%s := (%s == %s)" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Inequality of terms
  elif tp == LogEntry.OP_UNEQUAL:
    xs = entry.arguments
    pprint([
      "UNEQUAL",
      "%s = %s =/= %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Convert a number to float
  elif tp == LogEntry.OP_FLOAT:
    xs = entry.arguments
    pprint([
      "TO FLOAT",
      "%s = float( %s )" % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Power
  elif tp == LogEntry.OP_POW:
    xs = entry.arguments
    pprint([
      "POW",
      "%s = %s ** %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Bitmatch const true
  elif tp == LogEntry.OP_BITMATCH_CONST_TRUE:
    xs = entry.arguments
    pprint([
      "BITMATCH CONST TRUE",
      "%s:%s ++ %s = %s " % (pretty(xs[1]), pretty(xs[2]), pretty(xs[0]), pretty(xs[3]))
    ], tag)
  # Bitmatch const false
  elif tp == LogEntry.OP_BITMATCH_CONST_FALSE:
    xs = entry.arguments
    pprint([
      "BITMATCH CONST FALSE",
      "%s:%s is not head of %s " % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Bitmatch var true
  elif tp == LogEntry.OP_BITMATCH_VAR_TRUE:
    xs = entry.arguments
    pprint([
      "BITMATCH VAR TRUE",
      "%s with size %s + %s = %s " % (pretty(xs[0]), pretty(xs[2]), pretty(xs[1]), pretty(xs[3]))
    ], tag)
  # Tcons
  elif tp == LogEntry.OP_TCONS:
    xs = entry.arguments
    pprint([
      "TCONS",
      "%s = [ %s ] " % (pretty(xs[0]), pretty_list(xs[1:]))
    ], tag)
  # Less than (with integers)
  elif tp == LogEntry.OP_LT_INT:
    xs = entry.arguments
    pprint([
      "LT INT",
      "%s = %s < %s" % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Bitmach var false
  elif tp == LogEntry.OP_BITMATCH_VAR_FALSE:
    xs = entry.arguments
    pprint([
      "BITMATCH VAR FALSE",
      "%s with size %s" % (pretty(xs[1]), pretty(xs[0]))
    ], tag)
  # Cons
  elif tp == LogEntry.OP_CONS:
    xs = entry.arguments
    pprint([
      "CONS",
      "%s = [ %s | %s ] " % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Fun App
  elif tp == LogEntry.OP_LAMBDA:
    xs = entry.arguments
    pprint([
      "LAMBDA",
      "%s = apply(%s, [ %s ]) " % (pretty(xs[0]), pretty(xs[1]), pretty_list(xs[2:]))
    ], tag)
  # Is Function
  elif tp == LogEntry.OP_IS_FUN_WITH_ARITY:
    xs = entry.arguments
    pprint([
      "IS FUNCTION WITH ARITY",
      "%s = is_function(%s, %s) " % (pretty(xs[0]), pretty(xs[1]), pretty(xs[2]))
    ], tag)
  # Fresh lambda with arity
  elif tp == LogEntry.OP_FRESH_LAMBDA_WITH_ARITY:
    xs = entry.arguments
    pprint([
      "FRESH LAMBDA WITH ARITY",
      "is_function(%s, %s) " % (pretty(xs[0]), pretty(xs[1]))
    ], tag)
  # Evaluated Closure
  elif tp == LogEntry.OP_EVALUATED_CLOSURE:
    xs = entry.arguments
    pprint([
      "EVALUATED CLOSURE",
      "%s = apply(%s, [ %s ]) " % (pretty(xs[0]), pretty(xs[1]), pretty_list(xs[2:]))
    ], tag)
  else:
    print "UNKNOWN OPCODE", tp
    xs = entry.arguments
    print xs
    sys.exit(1)

if __name__ == "__main__":
  if "tty" in sys.argv:
    sys.argv.remove("tty")
  cglb.init()
  fname = sys.argv[1]
  n = 0
  for entry, rev in cio.JsonReader(fname, 100000000):
    n += 1
    print_cmd(entry, rev)
  print "Total Commands:", n
