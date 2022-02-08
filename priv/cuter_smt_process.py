# -*- coding: utf-8 -*-

import subprocess
from typing import Optional, Text

import attr

import cuter_logger
import cuter_smt_library as cslib


@attr.s(auto_attribs=True)
class Log:
  expr: cslib.Expr
  comment: Optional[Text] = None


class Solver:

  def __init__(self, arguments):
    """
    Create a subprocess using provided program arguments.
    """
    self.process = subprocess.Popen(
      arguments,
      stdin=subprocess.PIPE,
      stdout=subprocess.PIPE,
      stderr=subprocess.PIPE,
      universal_newlines=True
    )
    self.logger = cuter_logger.SMTLogger()
    self.logger.logComment(" ".join(arguments))

  def kill(self):
    self.process.kill()

  def write(self, log: Log):
    if isinstance(log, Log):
      line = cslib.serialize(log.expr)
      if log.comment:
        self.logger.logComment(log.comment)
    else:
      line = cslib.serialize(log)
    self.logger.log(line)
    self.process.stdin.write((line + "\n"))
    self.process.stdin.flush()

  def read(self):
    open_cnt = 0
    close_cnt = 0
    lines = []
    while True:
      line = self.process.stdout.readline()[:-1]
      lines.append(line)
      open_cnt += line.count("(")
      close_cnt += line.count(")")
      if open_cnt == close_cnt:
        break;
    smt = "\n".join(lines)
    self.logger.logComment(smt)
    if open_cnt == 0 and close_cnt == 0:
      return smt
    return cslib.unserialize(smt)

  def check_sat(self):
    self.write(["check-sat"])
    return self.read()

  def get_value(self, *expr):
    self.write(["get-value", list(expr)])
    return self.read()

  def exit(self):
    self.write(["exit"])


#class SolverCVC4(Solver):
#
# def __init__(self):
#   Solver.__init__(self, ["cvc4", "--lang=smt2", "--tlimit={}".format(timeout * 1000), "--fmf-fun", "--incremental"])
#   self.write(["set-logic", "UFDTLIRA"])


class SolverZ3(Solver):

  def __init__(self, timeout):
    Solver.__init__(self, ["z3", "-smt2", "-T:{}".format(timeout), "-in"])
