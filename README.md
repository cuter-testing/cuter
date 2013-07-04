conc-test
=========

How to compile & test
---------------------

*  Run `autoconf && ./configure`

*  Compile just the source files : `make fast`

*  Compile the source files & the tests & run dialyzer : `make`

*  Run the tests : `make utest`

*  Run the demo : `make demo`

*  All the above : `make all`

How to run
----------

You can simulate the execution of an MFA with `coordinator:run(Module, Function, Arguments).`

e.g. `erl -noinput -pa ebin -pa testsuite/ebin -eval "error_logger:tty(false)" -eval "coordinator:run(demo, foo, [1, 1])" -s init stop` after running `make`

