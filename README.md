[name pending]
==============

This is the source tree for *[name pending]*, a concolic testing tool
for the Erlang functional programming language.

Dependencies
------------

In order to use the tool, you need the following programs:

 1. *Erlang/OTP*

  Download the latest [Erlang/OTP distribution] [1] or clone the Erlang/OTP github repository:

        $ git clone git@github.com:erlang/otp.git

 2. *Python 2.x*

  Download the latest [Python 2.x distribution] [2]

 2. *Z3 Theorem Prover*

  Download the latest [Z3 distribution] [3] or clone the source git repository:

        $ git clone https://git01.codeplex.com/z3

  Make sure that the Z3Py (Python Interface) is installed.

Configuring & Building
----------------------

**Quick start**: the following gives you a default build:

    $ autoconf
    $ ./configure
    $ make depend
    $ make fast    # Can also use 'make -jX fast' for X number of jobs


The `make fast` step only compiles the necessary source files. For alternative
build commands you can use:

    $ make         # Compile the source files & the tests & run dialyzer
    $ make utest   # Compile the source files & the tests & run the tests
    $ make all     # Compile the source files & the tests & run dialyzer & the tests

Once you have a build you see the demo with:

    $ make demo


[1]:  http://www.erlang.org/            "www.erlang.org/"
[2]:  http://www.python.org/            "www.python.org/"
[3]:  http://z3.codeplex.com/           "z3.codeplex.com/"
