#!/usr/bin/env python
# -*- coding: utf-8 -*-

from z3 import *
from collections import defaultdict
import cuter_global as cglb
import cuter_logger as clg
import cuter_common as cc
import cuter_types as ctp

class Env:
  """
  The environment that maps symbolic varibles to their values and types
  and keeps track of the defined datatypes.
  """
  def __init__(self):
    self.paramsCnt = 0
    self.func_cnt = 0
    self.const_cnt = 0
    self.bitv_cnt = 0
    self.valueDb = {}
    self.params = []
    self.typeRoots = []
    self.typeDb = defaultdict(lambda: ctp.Type.generateAny())

  def addParam(self, x):
    self.params.append(x)

  def addRoot(self, x):
    self.typeRoots.append(x)

  def bind(self, s, v):
    self.valueDb[s] = v

  def bindType(self, s, tp):
    self.typeDb[s] = tp

  def lookup(self, x):
    e = self.valueDb
    return None if x not in e else e[x]

  def lookupType(self, x):
    return self.typeDb[x]

  def freshVar(self, s, Type):
    self.paramsCnt += 1
    x = Const("x%s" % self.paramsCnt, Type)
    self.valueDb[s] = x
    return x

  def justFreshVar(self, Type):
    self.paramsCnt += 1
    x = Const("x%s" % self.paramsCnt, Type)
    return x

  def generate_const(self, Type):
    self.const_cnt += 1
    x = Const("c%s" % self.const_cnt, Type)
    return x

  def generate_bitvec(self, size):
    self.bitv_cnt += 1
    x = BitVec("b%s" % self.bitv_cnt, size)
    return x

  def store_bits(self, s, bits):
    self.bve[s] = bits

  def generate_func(self, T1, T2):
    self.func_cnt += 1
    f = Function("f%s" % self.func_cnt, T1, T2)
    return f
    return t

  def generateBitvec(self, size):
    selfbitvCnt += 1
    bv = BitVec("b%s" % self.bitvCnt, size)
    return bv
