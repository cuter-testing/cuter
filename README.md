conc-test
=========

c(conc), c(conc_cserver), c(conc_load), c(conc_eval), c(conc_tserver), c(conc_lib).
conc:start(foo,mymin,[[a,b,c]]).
conc:start(foo,test,[[a,b,c]]).

beam_lib:chunks(code:which(lists), [compile_info]).
{ok,{_,[{compile_info,L}]}} = beam_lib:chunks(code:which(lists), [compile_info]), proplists:get_value(source, L).

