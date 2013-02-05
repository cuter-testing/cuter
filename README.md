conc-test
=========

*  Compile the source code and the demos with `make all`.  (`make` compiles just the source code.)

*  You can simulate the execution of an MFA with `coordinator:run(Module, Function, Arguments).`

	e.g. `coordinator:run(orddict, store, [key,val,orddict:new()]).`

*  Run the bencherl demos with `erl -pa demos/ebin -pa ebin/ -eval "coordinator:run_bencherl_demos()" -s init stop` (`ran` takes a while to execute...)

*  Run my demos with `erl -pa demos/ebin -pa ebin/ -eval "coordinator:run_my_demos()" -s init stop`

