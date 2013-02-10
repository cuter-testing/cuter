conc-test
=========

How to compile & test
---------------------

*  Compile just the source files : `make fast`

*  Compile the source files & the tests & run dialyzer : `make`

*  Run the tests : `make utest`

*  All the above : `make all`

How to run
----------

You can simulate the execution of an MFA with `coordinator:run(Module, Function, Arguments).`

e.g. `coordinator:run(orddict, store, [key,val,orddict:new()]).`

PS. You must start the interpreter with `erl -pa ebin`.

