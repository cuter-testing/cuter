#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

def init():
  global __TTY__
  global __RUN__
  
  __TTY__ = "tty" in sys.argv
  __RUN__ = True

