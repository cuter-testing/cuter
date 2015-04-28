#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

LISTS_FORALL_PATS = 1
LISTS_FORALL_NO_PATS = 2
LISTS_EXPAND = 3

def init():
  global __TTY__
  global __RUN__
  global __LISTS_INTERP__
  global __WPATS__
  
  __TTY__ = "tty" in sys.argv
  __RUN__ = True
  __LISTS_INTERP__ = LISTS_NO_PATS
  __WPATS__ = True

