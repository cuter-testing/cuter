conc-test
=========

*  Compile the source code and the demos with `make all`. You can simulate the execution of an MFA with `coord:run(Module, Function, Arguments).`

   e.g. `coord:run(orddict,store,[key,val,orddict:new()]).`

*  Run the bencherl demos with `erl -pa demos/ebin -eval "coord:run_bencherl_demos()" -s init stop` (ran takes a while to execute...)

*  Run my demos with `erl -pa demos/ebin -eval "coord:run_my_demos()" -s init stop`




