AC_INIT([cuter],[0.1])

###############################################
#
# Erlang compiler
#
###############################################
AC_ARG_WITH(erlc,
[AS_HELP_STRING([--with-erlc=ERLC_PATH],
		[specify the location of the Erlang compiler.])])

ERLC_PATH=`which erlc`
if test "x$with_erlc" != x; then
  ERLC_PATH="$with_erlc"
fi
AC_SUBST(ERLC_PATH)

cat > test_erlc.erl <<EOF
-module(test_erlc).
-export(@<:@foo/0@:>@).
foo() -> ok.
EOF

if $ERLC_PATH test_erlc.erl; then
  HAS_ERLC="1"
  rm -f test_erlc.beam
else
  HAS_ERLC="0"
fi
rm -f test_erlc.erl

if test "$HAS_ERLC" = "0"; then
   AC_MSG_ERROR([You need the Erlang Compiler. Please download Erlang at http://www.erlang.org/])
fi

AT_LEAST_21=
ERL_PATH=`which erl`
if $ERL_PATH -noshell -eval "X=erlang:system_info(otp_release) >= \"21\",X=true" -s init stop &> /dev/null; then
  AT_LEAST_21="-DAT_LEAST_21"
fi
AC_SUBST(AT_LEAST_21)

###############################################
#
# Python
#
###############################################
AC_ARG_WITH(python,
[AS_HELP_STRING([--with-python=PYTHON_PATH],
		[specify the location of the python 3.x executable.])])

PYTHON_PATH=`which python3`
if test "x$with_python" != x; then
  PYTHON_PATH="$with_python"
fi
AC_SUBST(PYTHON_PATH)

cat > test_python.py <<EOF
import sys
if sys.version_info < (3,6):
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
   AC_MSG_ERROR([You need Python >= 3.6. Please download Python at http://python.org])
fi

python_exec_prefix=`$PYTHON_PATH -c 'import sys; print(sys.exec_prefix)'`
if [[ -x "$python_exec_prefix/bin/protoc-gen-mypy" ]]; then
  PROTOC_GEN_MYPY="$python_exec_prefix/bin/protoc-gen-mypy"
else
  PROTOC_GEN_MYPY="`$PYTHON_PATH -m site --user-base`/bin/protoc-gen-mypy"
  if [[ ! -x $PROTOC_GEN_MYPY ]]; then
    AC_MSG_ERROR([protoc-gen-mypy binary not found. Please make sure that Python requirements have been setup])
  fi
fi
AC_SUBST(PROTOC_GEN_MYPY)

###############################################
#
# Z3
#
###############################################
AC_ARG_WITH(z3,
[AS_HELP_STRING([--with-z3=Z3_PATH],
		[specify the location of Z3.])])

Z3_PATH=`which z3`
if test "x$with_z3" != x; then
  Z3_PATH="$with_z3"
fi
AC_SUBST(Z3_PATH)

if $Z3_PATH --version >/dev/null 2>&1; then
  HAS_Z3="1"
  Z3_PATH=`which z3`
  Z3_VSN=`$Z3_PATH -version`
else
  HAS_Z3="0"
fi

if test "$HAS_Z3" = "0"; then
   AC_MSG_ERROR([You need Z3 >= 4.8.8. Please download Z3 at https://github.com/Z3Prover/z3 and follow the instructions.])
fi

###############################################
#
# Protobuf
#
###############################################
cat > test_protobuf.py <<EOF
try:
  __import__('google.protobuf')
except:
  exit(1)
else:
  exit(0)
EOF

if $PYTHON_PATH test_protobuf.py; then
  HAS_PROTOBUF="1"
  HAS_PROTOBUF_MSG="yes"
else
  HAS_PROTOBUF="0"
  HAS_PROTOBUF_MSG="no"
fi
rm -f test_protobuf.py

if test "$HAS_PROTOBUF" = "0"; then
  AC_MSG_ERROR([The Python protobuf library is missing. Please follow the instructions in the Python section of the README.md.])
fi

###############################################
#
# Protoc
#
###############################################
AC_ARG_WITH(protoc,
[AS_HELP_STRING([--with-protoc=PROTOC_PATH],
		[specify the location of the protoc executable.])])

PROTOC_PATH="protoc"
if test "x$with_protoc" != x; then
  PROTOC_PATH="$with_protoc"
fi
AC_SUBST(PROTOC_PATH)

cat > person.proto <<EOF
syntax = "proto3";
message Person { string name = 1; }
EOF

if $PROTOC_PATH --python_out=. person.proto; then
  HAS_PROTOC="1"
  rm -f person_pb2.py
else
  HAS_PROTOC="0"
fi
rm -f person.proto

if test "$HAS_PROTOC" = "0"; then
  AC_MSG_ERROR([protoc is missing. Please download version 22.0 of the package at https://github.com/google/protobuf/releases/tag/v22.0 and follow the instructions in the README. If you have a linux installation, then you can run ./fetch_protoc.sh and follow the instructions.])
fi

###############################################
#
# PropEr
#
###############################################
AC_ARG_WITH(proper,
[AS_HELP_STRING([--with-proper=PROPER],
		[specify the location of PropEr.])])

PROPER="lib/proper"
if test "x$with_proper" != x; then
  PROPER="$with_proper"
fi
AC_SUBST(PROPER)

cat > test_proper.erl <<EOF
-module(test_proper).
-export(@<:@foo/0@:>@).
foo() ->
  case code:which(proper) of
    non_existing -> error(non_existing);
    _ -> ok
  end.
EOF

if $ERLC_PATH test_proper.erl && $ERL_PATH -noshell -pa $PROPER/ebin -eval "test_proper:foo()" -s init stop &> /dev/null; then
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
  AC_MSG_NOTICE([PropEr is missing. You will not be able to run the tests. Download it at https://github.com/proper-testing/proper and follow the instructions.])
fi

###############################################
#
# GPB
#
###############################################
AC_ARG_WITH(gpb,
[AS_HELP_STRING([--with-gpb=GPB],
		[specify the location of gbp.])])

GPB="lib/gpb"
if test "x$with_gpb" != x; then
  GPB="$with_gpb"
fi
AC_SUBST(GPB)

cat > message.proto <<EOF
message Person {
  required string name = 1;
  required int32 id = 2;
  optional string email = 3;
}
EOF

PROTOCERL="$GPB/bin/protoc-erl"
if $PROTOCERL -I. message.proto &> /dev/null; then
  HAS_GPB="1"
  HAS_GPB_MSG="yes"
else
  HAS_GPB="0"
  HAS_GPB_MSG="unknown"
fi
rm -f message.proto message.erl message.hrl

if test "$HAS_GPB" = "0"; then
  GPB=
  AC_MSG_NOTICE([GPB is missing. Run: git submodule update --init && git submodule foreach make, then re-run the configure script.])
fi

###############################################
#
# Generate configuration
#
###############################################
AC_CONFIG_FILES([Makefile])
AC_OUTPUT

###############################################
#
# Show information on how to build CutEr
#
###############################################
cat <<EOF

CutEr was configured with success.
  Erlang:	      $ERL_PATH
  Erlang Compiler:    $ERLC_PATH
  Z3:                 $Z3_PATH	($Z3_VSN)
  Python:             $PYTHON_PATH
  protoc:             $PROTOC_PATH
  protobuf (Python):  $HAS_PROTOBUF_MSG
  GPB:                $HAS_GPB_MSG
  PropEr:             $HAS_PROPER_MSG

To build CutEr, execute:
  make depend
  make
EOF
