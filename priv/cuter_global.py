#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

def init():
  global __TTY__
  global __RUN__
  global __WPATS__
  
  __TTY__ = "tty" in sys.argv
  __RUN__ = True
  __WPATS__ = True

