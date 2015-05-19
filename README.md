CutEr
=====

This is the source tree for CutEr, a concolic unit testing tool for Erlang.

[![Build Status](https://travis-ci.org/aggelgian/cuter.svg?branch=master)](https://travis-ci.org/aggelgian/cuter)

***CAUTION: This tool is still under heavy development (and not yet released!)***

* [Dependencies](#dependencies)
* [Installing](#installing)
* [Usage](#usage)

Dependencies
------------

In order to use the tool, you need the following programs:

1. **Erlang/OTP**

  Note that using a pre-built package or binaries will not suffice if the library's modules have not been compiled with debug information.
  In that case, you will need to build and install Erlang/OTP from source.

  Download the latest [Erlang/OTP source code](http://www.erlang.org/download.html) or clone the Erlang/OTP github repository:

        git clone https://github.com/erlang/otp.git

  Then follow the instructions in [INSTALL.md](https://github.com/erlang/otp/blob/maint/HOWTO/INSTALL.md) on building and
  installing Erlang/OTP.

2. **Python 2.x**

  Download and install the latest [Python 2.x distribution](http://www.python.org).

3. **Z3 Theorem Prover**

  Download the latest [Z3 stable distribution](https://github.com/Z3Prover/z3) or clone the master branch of the source git repository:

        git clone https://github.com/Z3Prover/z3.git

  To install Z3, follow the instructions in the [README file](https://github.com/Z3Prover/z3/blob/master/README).
  Also, make sure that Z3Py (Python Interface) is installed.

Installing
----------

* Download CutEr's sources or clone this repository:

        git clone https://github.com/aggelgian/cuter.git

* Configure and compile CutEr. For a default build:

        autoconf
        ./configure
        make depend
        make

* **[Optional]** If you want to run the full test suite of CutEr, you will also need [PropEr](https://github.com/manopapad/proper)
  and [meck](https://github.com/eproxus/meck). You can install them independently or go to CutEr's base directory and run:

        git submodule init && git submodule update
        git submodule foreach make

  Subsequently, you can build and run the unit tests with `make utest`

* **[Optional]**  You can also run Dialyzer with `make dialyzer`

* Add CutEr's base directory to your Erlang library path by updating the `ERL_LIBS` environment variable. Just add

        export ERL_LIBS=/full/path/to/cuter:$ERL_LIBS

  to your shell startup file (e.g. `~/.bashrc` for Bash).

Usage
-----

Let's say that you just wrote a simple module `foo` that just contains the exported function `bar/2`.
The source file `foo.erl` is:

```erlang
-module(foo).
-export([bar/2]).

-spec bar([number()], [number()]) -> number().
bar([], Ys) -> lists:sum(Ys);
bar([X|Xs], [Y|Ys]) -> X * Y + bar(Xs, Ys).
```

Go to the directory of the source file and compile it with debug information:

    erlc +debug_info foo.erl

In order to test `foo:bar/2` with CutEr, you will need a well-formed input that will act as a seed.
Let that be `foo:bar([1], [2])`.

CutEr can be invoked by calling the `cuter:run/3` function, that is:

    erl -noshell -eval "cuter:run(foo, bar, [[1], [2]])" -s init stop

and it reports a list of inputs that lead to runtime errors, for example `[1.0,2.0,0.0], [0,1]`.

To sum up, `cuter:run/3` is called as `cuter:run(M, F, As)` where

* `M` is the module
* `F` is the function
* `As` is the list of arguments of the seed input (indirectly denotes the arity of `F`)

CutEr provides more API functions that also come with options that control the concolic execution of Erlang programs.
These will be explained in a set of forthcoming tutorials.  In the meantime, you can find out about them by browsing
the source code of CutEr.  Have fun with the tool!
