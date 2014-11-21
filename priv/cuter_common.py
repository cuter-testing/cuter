#!/usr/bin/env python
# -*- coding: utf-8 -*-

from cuter_global import *

JSON_TYPE_ANY = 0
JSON_TYPE_INT = 1
JSON_TYPE_FLOAT = 2
JSON_TYPE_ATOM = 3
JSON_TYPE_LIST = 4
JSON_TYPE_TUPLE = 5
JSON_TYPE_PID = 6
JSON_TYPE_REF = 7

CMD_LOAD_TRACE_FILE = 1
CMD_SOLVE = 2
CMD_GET_MODEL = 3
CMD_ADD_AXIOMS = 4
CMD_FIX_VARIABLE = 5
CMD_RESET_SOLVER = 6
CMD_STOP = 42

RSP_MODEL_DELIMITER_START = "model_start"
RSP_MODEL_DELIMITER_END = "model_end"

CONSTRAINT_TRUE = 1
CONSTRAINT_FALSE = 2

OP_PARAMS = 1
# OP_SPEC = 2
OP_GUARD_TRUE = 3
OP_GUARD_FALSE = 4
OP_MATCH_EQUAL_TRUE = 5
OP_MATCH_EQUAL_FALSE = 6
OP_TUPLE_SZ = 7
OP_TUPLE_NOT_SZ = 8
OP_TUPLE_NOT_TPL = 9
OP_LIST_NON_EMPTY = 10
OP_LIST_EMPTY = 11
OP_LIST_NOT_LST = 12
OP_SPAWN = 13
OP_SPAWNED = 14
OP_MSG_SEND = 15
OP_MSG_RECEIVE = 16
OP_MSG_CONSUME = 17
OP_UNFOLD_TUPLE = 18
OP_UNFOLD_LIST = 19

OP_HD = 25
OP_TL = 26
OP_IS_INTEGER = 27
OP_IS_ATOM = 28
OP_IS_FLOAT = 29
OP_IS_LIST = 30
OP_IS_TUPLE = 31
OP_IS_BOOLEAN = 32
OP_IS_NUMBER = 33
OP_PLUS = 34
OP_MINUS = 35
OP_TIMES = 36
OP_RDIV = 37
OP_IDIV_NAT = 38
OP_REM_NAT = 39

def is_constraint_kind(tp):
  return tp == CONSTRAINT_TRUE or tp == CONSTRAINT_FALSE

def is_interpretable(tp):
  xs = set([OP_SPAWN, OP_SPAWNED, OP_MSG_SEND, OP_MSG_RECEIVE, OP_MSG_CONSUME])
  return tp not in xs

def is_reversible_bif(tp):
  x = {
    OP_HD: True,
    OP_TL: True,
    OP_IS_INTEGER: False,
    OP_IS_ATOM: False,
    OP_IS_FLOAT: False,
    OP_IS_LIST: False,
    OP_IS_TUPLE: False,
    OP_IS_BOOLEAN: False,
    OP_IS_NUMBER: False,
    OP_PLUS: True,
    OP_MINUS: True,
    OP_TIMES: True,
    OP_RDIV: True,
    OP_IDIV_NAT: True,
    OP_REM_NAT: True,
  }
  return x[tp] if tp in x else False

def is_reversible(tp, opcode):
  return is_constraint_kind(tp) or is_reversible_bif(opcode)
