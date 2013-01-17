conc-test-ref
=============

c(rush), c(foo), c(conc), c(conc_cserver), c(conc_load), c(conc_eval), c(conc_tserver), c(conc_lib).
c(rush), c(foo), c(conc), c(conc_cserver), c(conc_load), c(conc_eval), c(conc_tserver), c(conc_lib), c(bin_lib).
f(), S = now(), c(bin_lib), E = now(), T = timer:now_diff(E, S)/1000000, io:format("~w secs~n", [T]), f().
c(rush), c(foo), c(conc,[native]), c(conc_cserver,[native]), c(conc_load,[native]), c(conc_eval,[native]), c(conc_tserver,[native]), c(conc_lib,[native]).
conc:run(foo,mymin,[[a,b,c]]).
conc:run(foo,spawn,[lists,reverse,[[1,2,3]]]).
conc:run(rush, solve, [6,6,{2,0},[{0,{2,3},{2,4}},{1,{0,0},{2,0}},{2,{4,0},{4,1}},{3,{5,1},{5,3}},{4,{1,2},{3,2}},{5,{0,4},{0,5}},{6,{1,5},{3,5}},{7,{4,5},{5,5}}]]).
conc:run(rush,solve,[6,6,{5,2},[{0,{0,2},{1,2}},{1,{0,3},{1,3}},{2,{2,3},{3,3}},{3,{1,4},{2,4}},{4,{3,4},{3,5}},{5,{4,5},{5,5}},{6,{5,3},{5,4}},{7,{4,2},{4,4}},{8,{2,0},{3,0}},{9,{2,1},{3,1}},{10,{4,0},{5,0}},{11,{4,1},{5,1}}]]).

errors
------
{undef, M}
{cover_compiled, M}
{undef, FuncKey}
{not_exported, MFA}
{lambda_fun_argument_limit, Arity}
{invalid_timeout, Timeout}


How to Run
----------

a) You can run from within the interpreter

Compile with:
```erlang
c(conc,[native]),c(conc_cserver,[native]),c(conc_load,[native]),c(conc_eval,[native]),c(conc_tserver,[native]),c(conc_lib).
```

You can run with
```erlang
conc:run(Module, Function, Arguments).
```

or edit the M,F,A variables in conc.erl, recompile conc.erl and run
```erlang
conc:run().
```

b) You can run from the Unix command line

First edit the M,F,A variables in conc.erl
Then compile with
```
make
```
And run with
```
erl -noshell -s conc run -s init stop
```
