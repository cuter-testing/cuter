# -*- coding: utf-8 -*-

import sys

LISTS_FORALL_PATS = 1
LISTS_FORALL_NO_PATS = 2
LISTS_EXPAND = 3

__RUN__ = False

def init(debug=False):
    global __TTY__
    global __RUN__
    global __LISTS_INTERP__
    global __WPATS__
    global __LOG_DATA_RECEIVED__
    global __LOG_JSON_LOADED__
    global __LOG_MODEL_UNKNOWN__
    global __LOG_DEBUG_INFO__
    global __LOG_SMT__

    __TTY__ = "tty" in sys.argv
    __RUN__ = True
    __LISTS_INTERP__ = LISTS_FORALL_NO_PATS
    __WPATS__ = True

    # Logging flags
    __LOG_DEBUG_INFO__ = True
    __LOG_DATA_RECEIVED__ = False
    __LOG_JSON_LOADED__ = False
    __LOG_MODEL_UNKNOWN__ = False
    __LOG_SMT__ = debug

