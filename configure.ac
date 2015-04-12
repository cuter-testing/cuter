AC_INIT([cuter], [0.1])

###############################################
#
# Testing erlang compiler
#
###############################################
AC_ARG_WITH(erlc,
[AS_HELP_STRING([--with-erlc=ERLC],
		[specify the location of the erlang compiler.])])

ERLC="erlc"
if test "x$with_erlc" != x; then
  ERLC="$with_erlc"
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

if test "$HAS_ERLC" = "0"; then 
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

PYTHON_PATH="python"
if test "x$with_python" != x; then
  PYTHON_PATH="$with_python"
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
   AC_MSG_ERROR([You need Z3 and Z3Py. Please download Z3 at https://github.com/Z3Prover/z3 and follow the instructions.])
fi

###############################################
#
# Testing meck
#
###############################################
AC_ARG_WITH(meck,
[AS_HELP_STRING([--with-meck=MECK],
		[specify the location of meck.])])

MECK="lib/meck/ebin/"
if test "x$with_meck" != x; then
  MECK="$with_meck/ebin/"
fi

AC_SUBST(MECK)

cat > test_meck.erl <<EOF
-module(test_meck).
-compile(export_all).
foo() ->
  case code:which(meck) of
    non_existing -> error(non_existing);
    _ -> ok
  end.
EOF

ERL=`which erl`
if $ERLC test_meck.erl && $ERL -noshell -pa $MECK -eval "test_meck:foo()" -s init stop &> /dev/null; then
  HAS_MECK="1"
  HAS_MECK_MSG="yes"
else
  HAS_MECK="0"
  HAS_MECK_MSG="no"
  rm -f erl_crash.dump
fi
rm -f test_meck.erl test_meck.beam

if test "$HAS_MECK" = "0"; then
   MECK=
   AC_MSG_NOTICE([Meck is missing. You will not be able to run some of the tests. Download it at https://github.com/eproxus/meck.])
fi

###############################################
#
# Testing proper
#
###############################################
cat > test_proper.erl <<EOF
-module(test_proper).
-compile(export_all).
foo() ->
  case code:which(proper) of
    non_existing -> error(non_existing);
    _ -> ok
  end.
EOF

ERL=`which erl`
if $ERLC test_proper.erl && $ERL -noshell -eval "test_proper:foo()" -s init stop &> /dev/null; then
  HAS_PROPER="1"
  HAS_PROPER_MSG="yes"
else
  HAS_PROPER="0"
  HAS_PROPER_MSG="unknown"
  rm -f erl_crash.dump
fi
rm -f test_proper.erl test_proper.beam

if test "$HAS_PROPER" = "0"; then
   PROPER=
   AC_MSG_NOTICE([The path to PropEr is unknown. If you have installed it make sure you have included it in your ERL_LIBS path. If not, you can find it at https://github.com/manopapad/proper and follow the instructions.])
fi


###############################################
#
# Generating configuration
#
###############################################
AC_OUTPUT(Makefile)

###############################################
#
# Show Information on how to build CutEr
#
###############################################
cat <<EOF

CutEr was configured with success.
Erlang Compiler:    $ERLC
Python:             $PYTHON_PATH
Z3Py:               $HAS_Z3_MSG
Meck:               $HAS_MECK_MSG
Proper:             $HAS_PROPER_MSG

To build CutEr, execute:
  make depend
  make
EOF
