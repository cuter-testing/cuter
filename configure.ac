AC_INIT([concolic], [0.1])

###############################################
#
# Testing erlang compiler
#
###############################################
AC_ARG_WITH(erlc,
[AS_HELP_STRING([--with-erlc=ERLC],
		[specify the location of the erlang compiler.])])

if test "x$with_erlc" != x; then
  ERLC="$with_erlc"
else
  ERLC=`which erlc`
fi

AC_SUBST(ERLC)

cat > test_erlc.erl <<EOF
-module(test_erlc).
-compile(export_all).
foo() -> ok.
EOF

if $ERLC test_erlc.erl; then
  HAS_ERLC="1"
  rm -f test_erlc.beam
else
  HAS_ERLC="0"
fi
rm -f test_erlc.erl

if test "$ERLC" = ""; then 
   AC_MSG_ERROR([You need the Erlang Compiler. Please download Erlang at http://www.erlang.org/])
elif test "$HAS_ERLC" = "0"; then 
   AC_MSG_ERROR([You need the Erlang Compiler. Please download Erlang at http://www.erlang.org/])
fi

###############################################
#
# Testing python
#
###############################################
AC_ARG_WITH(python,
[AS_HELP_STRING([--with-python=PYTHON_PATH],
		[specify the location of the python 2.x executable.])])

if test "x$with_python" != x; then
  PYTHON_PATH="$with_python"
else
  PYTHON_PATH=`which python`
fi

AC_SUBST(PYTHON_PATH)

cat > test_python.py <<EOF
from sys import version
if version >= "3":
   exit(1)
exit(0)
EOF

if $PYTHON_PATH test_python.py; then
  HAS_PYTHON="1"
else
  HAS_PYTHON="0"
fi
rm -f test_python.py

if test "$HAS_PYTHON" = "0"; then 
   AC_MSG_ERROR([You need Python 2.x. Please download Python at http://python.org])
fi


###############################################
#
# Testing Z3Py
#
###############################################
cat > test_z3.py <<EOF
try:
  __import__('z3')
except:
  exit(1)
else:
  exit(0)
EOF

if $PYTHON_PATH test_z3.py; then
  HAS_Z3="1"
  HAS_Z3_MSG="yes"
else
  HAS_Z3="0"
  HAS_Z3_MSG="no"
fi
rm -f test_z3.py

if test "$HAS_Z3" = "0"; then 
   AC_MSG_ERROR([You need Z3 and Z3Py. Please download Z3 at http://z3.codeplex.com])
fi

###############################################
#
# Generating configuration
#
###############################################
AC_OUTPUT(Makefile)

###############################################
#
# Show Information on how to build Concolic
#
###############################################
cat <<EOF

Concolic was configured with success.
Erlang Compiler:    $ERLC
Python:             $PYTHON_PATH
Z3Py:               $HAS_Z3_MSG

To build Concolic, execute:
  make fast
EOF
