#!/usr/bin/env python
# -*- coding: utf-8 -*-

import datetime
import json
import os

import cuter_global as cglb

DATA_RECEIVED_LOG = "data_received.log"
JSON_LOADED_LOG = "json_loaded.log"
MODEL_UNKNOWN = "model_unknown.log"
DEBUG_LOG = "debug.log"

def touch(fname):
  if os.path.exists(fname):
    os.utime(fname, None)
  else:
    open(fname, 'a').close()

class Logger(object):
  def __init__(self, fname = None):
    if fname:
      if os.path.isfile(fname):
        self.fd = open(fname, "a")
      else:
        self.fd = open(fname, "w")
    else:
      self.fd = None

  def log(self, msg):
    if self.fd:
      self.fd.write(msg + "\n")


class SMTLogger(Logger):
  def __init__(self):
    if not cglb.__LOG_SMT__:
      super(SMTLogger, self).__init__()
    else:
      n = 1
      while os.path.exists("smt.log.{:05d}".format(n)):
        n += 1
      fname = "smt.log.{:05d}".format(n)
      touch(fname)
      super(SMTLogger, self).__init__(fname)

  def logComment(self, msg):
    self.log("; " + msg)


def debug_info(data):
  if cglb.__LOG_DEBUG_INFO__:
    fd = open(DEBUG_LOG, "a")
    fd.write("{}\n".format(data))
    fd.flush()
    fd.close()

def data_received(data):
  if cglb.__LOG_DATA_RECEIVED__:
    fd = open(DATA_RECEIVED_LOG, "a")
    fd.write("{}\n".format(data))
    fd.flush()
    fd.close()

def json_loaded(i, entry, rev):
  if cglb.__LOG_JSON_LOADED__:
    fd = open(JSON_LOADED_LOG, "a")
    fd.write("{}.\n".format(i))
    fd.write("  OPCODE {}\n".format(entry.type))
    fd.write("  IS_CONSTRAINT {}\n".format(entry.is_constraint))
    fd.write("  TAG {}\n".format(entry.tag))
    fd.write("  ARGS {}\n".format(str(entry.arguments)))
    fd.write("  REVERSIBLE {}\n".format(rev))
    fd.flush()
    fd.close()

def model_unknown(axs):
  if cglb.__LOG_MODEL_UNKNOWN__:
    fd = open(MODEL_UNKNOWN, "a")
    fd.write("{}\n".format(datetime.datetime.now()))
    fd.write("  AXS {}\n".format(axs))
    fd.flush()
    fd.close()

def clean_empty_logs():
  clean_empty_log(DATA_RECEIVED_LOG)
  clean_empty_log(JSON_LOADED_LOG)
  clean_empty_log(MODEL_UNKNOWN)

def clean_empty_log(log):
  try:
    if os.stat(log).st_size == 0:
      os.remove(log)
  except:
    pass
