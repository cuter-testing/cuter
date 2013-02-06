conc-test
=========

*  Compile the source code and the tests and run dialyzer and the tests with `make all`.  (`make fast` compiles just the source code.)

*  You can simulate the execution of an MFA with `coordinator:run(Module, Function, Arguments).`

	e.g. `coordinator:run(orddict, store, [key,val,orddict:new()]).`


